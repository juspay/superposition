"""The flag type contract, shared with the Rust and Java clients.

No coercion. An integer widens to a float; nothing else converts. Every case below is a
value some client used to read as the wrong type — Python coerced "true" and 1 to booleans,
Java truncated 1.5 to an integer, and Rust turned a top-level array into an index-keyed map.

These exercise the extractors directly, so no server or FFI cache is needed.
"""

import json
from typing import Any, Dict, List, Optional

from openfeature.evaluation_context import EvaluationContext
from openfeature.exception import ErrorCode

from superposition_provider.interfaces import AllFeatureProvider

FLAGS = {
    "currency": "Rupee",
    "price": 10000,
    "ratio": 1.5,
    "enabled": True,
    "tags": ["a", "b"],
    "meta": {"tier": "gold", "credits": 5},
}


class StubProvider(AllFeatureProvider):
    """Serves a fixed flag set, mirroring what the FFI cache hands back."""

    def resolve_all_features_with_filter(
        self,
        context: Optional[EvaluationContext] = None,
        prefix_filter: Optional[List[str]] = None,
    ) -> Dict[str, Any]:
        # The real provider json.loads the FFI's JSON-encoded strings; round-trip to match.
        return {k: json.loads(json.dumps(v)) for k, v in FLAGS.items()}

    async def resolve_all_features_with_filter_async(
        self,
        context: Optional[EvaluationContext] = None,
        prefix_filter: Optional[List[str]] = None,
    ) -> Dict[str, Any]:
        return self.resolve_all_features_with_filter(context, prefix_filter)


provider = StubProvider()
ctx = EvaluationContext()


def test_values_resolve_as_their_own_type():
    assert provider.resolve_string("currency", "none", ctx).value == "Rupee"
    assert provider.resolve_int("price", 0, ctx).value == 10000
    assert provider.resolve_float("ratio", 0.0, ctx).value == 1.5
    assert provider.resolve_bool("enabled", False, ctx).value is True
    assert provider.resolve_object("meta", {}, ctx).value == {"tier": "gold", "credits": 5}
    assert provider.resolve_object("tags", [], ctx).value == ["a", "b"]


def test_an_integer_widens_to_a_float():
    # Lossless, and the one conversion all three clients allow.
    assert provider.resolve_float("price", 0.0, ctx).value == 10000.0


def test_a_float_does_not_narrow_to_an_integer():
    # Java used to truncate this to 1.
    result = provider.resolve_int("ratio", 0, ctx)
    assert result.error_code == ErrorCode.TYPE_MISMATCH
    assert result.value == 0


def test_strings_and_numbers_are_not_booleans():
    # _to_bool used to accept "true" and any non-zero number.
    assert provider.resolve_bool("currency", False, ctx).error_code == ErrorCode.TYPE_MISMATCH
    assert provider.resolve_bool("price", False, ctx).error_code == ErrorCode.TYPE_MISMATCH


def test_a_primitive_is_not_an_object():
    assert provider.resolve_object("currency", {}, ctx).error_code == ErrorCode.TYPE_MISMATCH


def test_a_number_is_not_a_string():
    assert provider.resolve_string("price", "none", ctx).error_code == ErrorCode.TYPE_MISMATCH


def test_a_missing_flag_is_flag_not_found_not_a_type_mismatch():
    result = provider.resolve_string("nope", "fallback", ctx)
    assert result.error_code == ErrorCode.FLAG_NOT_FOUND
    assert result.value == "fallback"


def test_the_default_is_returned_on_every_error():
    assert provider.resolve_bool("currency", True, ctx).value is True
    assert provider.resolve_int("ratio", 42, ctx).value == 42


if __name__ == "__main__":
    # The package has no pytest dependency, so these run standalone too.
    cases = [v for k, v in sorted(globals().items()) if k.startswith("test_")]
    for case in cases:
        case()
        print(f"ok  {case.__name__}")
    print(f"\n{len(cases)} passed")
