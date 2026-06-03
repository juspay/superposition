#!/usr/bin/env python3
"""
Generate the Hyperswitch SuperTOML config from the upstream development.toml.

Repeatable: identical input -> identical output (deterministic sort orders
across dimensions, configs, and overrides). Designed to support a phased
rollout where individual source sections move into SuperTOML one at a time.

Usage:
    # Default: regenerate the SuperTOML beside this script.
    python3 generate_hyperswitch_supertoml.py

    # Pin source/output paths explicitly.
    python3 generate_hyperswitch_supertoml.py \\
        --source /tmp/hyperswitch-dev.toml \\
        --output ./hyperswitch-development.toml

    # Phase the migration - only convert a subset of sections.
    python3 generate_hyperswitch_supertoml.py \\
        --sections pm_filters,connector_flags

    # Re-validate an existing SuperTOML against the source without rewriting.
    python3 generate_hyperswitch_supertoml.py --validate-only

Validation runs after every generate:
  1. Output re-parses cleanly as TOML, and (when superposition_bindings is
     installed) also as SuperTOML via ffi_parse_toml_config - the same Rust
     code superposition_core uses in production. Surfaces JSON Schema
     violations that pure-Python TOML can't see.
  2. Every override's dimensions are declared and values are in their enums.
  3. A built-in parity matrix (~28 cases) resolves to the values the source
     specifies. When bindings are available, resolution routes through
     ffi_eval_config; otherwise falls back to a pure-Python cohort-aware
     resolver that mirrors the same priority sum logic. Use --no-bindings to
     force the fallback for testing.

Adding a new section: register it under `@section("name")` and update the
DEFAULT_SECTIONS list. Validation will catch any references it produces to
undeclared enum members, so drift in the upstream file surfaces loudly.
"""

from __future__ import annotations

import argparse
import json
import sys
import tomllib
from collections import OrderedDict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Callable

# Optional: use the official Rust-backed Python bindings if available. Falls
# back to a built-in TOML parser + Python cohort-aware resolver so the script
# is still useful on machines that don't have the native lib installed.
#
# The published `superposition_bindings` wheel on PyPI currently only ships
# metadata - the Python module and platform-tagged .so live in the source
# tree at clients/python/bindings/. If we're running from inside this repo,
# add that directory to sys.path so import works without requiring the user
# to set PYTHONPATH manually. Building the .so:
#   cargo build --release -p superposition_core
#   cp target/release/libsuperposition_core.so \\
#      clients/python/bindings/superposition_bindings/\\
#      libsuperposition_core-x86_64-unknown-linux-gnu.so

def _locate_bindings_in_tree() -> "Path | None":
    here = Path(__file__).resolve()
    for parent in [here, *here.parents]:
        candidate = parent / "clients" / "python" / "bindings"
        if (candidate / "superposition_bindings" / "superposition_client.py").is_file():
            return candidate
    return None


_bindings_dir = _locate_bindings_in_tree()
if _bindings_dir and str(_bindings_dir) not in sys.path:
    sys.path.insert(0, str(_bindings_dir))

try:
    from superposition_bindings.superposition_client import (
        ffi_parse_toml_config as _ffi_parse_toml_config,
        ffi_eval_config as _ffi_eval_config,
        MergeStrategy as _BindingsMergeStrategy,
    )
    BINDINGS_AVAILABLE = True
    BINDINGS_LOAD_ERROR = None
except Exception as _e:  # ImportError, FileNotFoundError, RuntimeError, ...
    _ffi_parse_toml_config = None
    _ffi_eval_config = None
    _BindingsMergeStrategy = None
    BINDINGS_AVAILABLE = False
    BINDINGS_LOAD_ERROR = repr(_e)

# ---------------------------------------------------------------------------
# Constants - position weights, region cohort definitions.
# ---------------------------------------------------------------------------

POSITIONS = {
    "payment_type": 7,
    "capture_method": 6,
    "connector": 5,
    "country": 4,
    "payment_method": 3,
    "currency": 2,
    "region": 1,
}

REGIONS: "OrderedDict[str, list[str]]" = OrderedDict([
    ("europe", ["AT", "BE", "CH", "CZ", "DE", "DK", "ES", "FI", "FR", "GB",
                "GR", "HU", "IE", "IT", "NL", "NO", "PL", "PT", "RO", "SE", "SK"]),
    ("north_america", ["US", "CA", "MX"]),
    ("apac", ["AU", "CN", "HK", "ID", "IN", "JP", "KR", "MY", "NZ", "PH",
              "SG", "TH", "TW", "VN"]),
    ("latam", ["BR"]),
    ("mea", ["ZA"]),
])
REGION_OF = {c: r for r, cs in REGIONS.items() for c in cs}

# `card` / `bank_debit` in temp_locker_enable_config are umbrella categories.
PM_UMBRELLA = {
    "card": ["credit", "debit"],
    "bank_debit": ["ach", "sepa"],
    "bank_transfer": ["bank_transfer"],
    "wallet": [],  # too broad; left empty
}

# Country/currency lists in the source that exceed these thresholds are
# "essentially anywhere"; we drop the corresponding dimension from the override
# context. The hand-written conversion did this implicitly for credit/debit.
# Lower the thresholds to keep more fidelity; raise to keep the override count
# small. Pairs where this triggered are surfaced in the run summary so they
# can be refined later in the phased migration.
COUNTRY_THRESHOLD = 50
CURRENCY_THRESHOLD = 30

DEFAULT_SECTIONS = [
    "pm_filters",
    "mandates",
    "payout_method_filters",
    "bank_config",
    "connectors_base_url",
    "connector_flags",
    "tokenization",
    "temp_locker",
]


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def split_csv(s: Any) -> "list[str]":
    if not isinstance(s, str):
        return []
    return sorted({x.strip() for x in s.split(",") if x.strip()})


def expand_country_currency(countries: "list[str]", currencies: "list[str]") -> "list[dict]":
    """Build minimal context fragments. Collapse to `region` cohort when the
    country list exactly matches one of the declared regions."""
    cs = set(countries)
    if cs:
        match = next((r for r, v in REGIONS.items() if set(v) == cs), None)
        country_frags = [{"region": match}] if match else [{"country": c} for c in sorted(cs)]
    else:
        country_frags = [{}]
    currency_frags = [{"currency": c} for c in sorted(set(currencies))] if currencies else [{}]
    return [{**cf, **ccf} for cf in country_frags for ccf in currency_frags]


def toml_value(v: Any) -> str:
    """Emit a TOML expression. Strings -> basic strings (JSON-encoded).
    Inline tables (dict) and arrays (list) handled recursively."""
    if isinstance(v, bool):
        return "true" if v else "false"
    if v is None:
        raise ValueError("None has no TOML representation")
    if isinstance(v, str):
        return json.dumps(v)
    if isinstance(v, (int, float)):
        return repr(v)
    if isinstance(v, list):
        return "[" + ", ".join(toml_value(x) for x in v) + "]"
    if isinstance(v, dict):
        items = [f"{k} = {toml_value(vv)}" for k, vv in v.items()]
        return "{ " + ", ".join(items) + " }"
    raise TypeError(f"Cannot encode {type(v).__name__}")


def sort_context(ctx: dict) -> dict:
    """Sort context keys by descending position weight, so the most specific
    dimension shows up first - matches the hand-written file's reading order."""
    return OrderedDict(
        sorted(ctx.items(), key=lambda kv: (-POSITIONS.get(kv[0], 0), kv[0]))
    )


# ---------------------------------------------------------------------------
# Accumulator + section registry
# ---------------------------------------------------------------------------

@dataclass
class Accumulator:
    connectors: "set[str]" = field(default_factory=set)
    payment_methods: "set[str]" = field(default_factory=set)
    countries: "set[str]" = field(default_factory=set)
    currencies: "set[str]" = field(default_factory=set)
    overrides: "list[dict]" = field(default_factory=list)
    extra_configs: "dict[str, dict]" = field(default_factory=dict)
    sections_run: "list[str]" = field(default_factory=list)
    # Rows where thresholds collapsed country/currency lists to a global enable
    # for the (connector, payment_method) pair. Reported in the run summary.
    collapsed_rows: "list[str]" = field(default_factory=list)

    def add_override(self, ctx: dict, **values: Any) -> None:
        self.overrides.append({"_context_": sort_context(ctx), **values})

    def add_config(self, key: str, value: Any, schema: dict) -> None:
        self.extra_configs[key] = {"value": value, "schema": schema}


SECTIONS: "dict[str, Callable[[dict, Accumulator], None]]" = {}


def section(name: str):
    def deco(fn):
        SECTIONS[name] = fn
        return fn
    return deco


# ---------------------------------------------------------------------------
# Section converters
# ---------------------------------------------------------------------------

def _maybe_collapse(countries: "list[str]", currencies: "list[str]",
                    *, label: str, acc: Accumulator) -> "tuple[list[str], list[str]]":
    """Apply COUNTRY_THRESHOLD / CURRENCY_THRESHOLD heuristics; record any
    collapse so the user knows where fidelity was traded for compactness."""
    if len(countries) > COUNTRY_THRESHOLD:
        acc.collapsed_rows.append(f"{label}: {len(countries)} countries -> dropped country dim")
        countries = []
    if len(currencies) > CURRENCY_THRESHOLD:
        acc.collapsed_rows.append(f"{label}: {len(currencies)} currencies -> dropped currency dim")
        currencies = []
    return countries, currencies


@section("pm_filters")
def convert_pm_filters(source: dict, acc: Accumulator) -> None:
    """[pm_filters.<connector>] / [pm_filters.default] - emits `enabled` and
    capture_method disable overrides."""
    for connector, rows in source.get("pm_filters", {}).items():
        is_default = (connector == "default")
        if not is_default:
            acc.connectors.add(connector)
        if not isinstance(rows, dict):
            continue
        for pm, spec in rows.items():
            acc.payment_methods.add(pm)
            if not isinstance(spec, dict):
                continue
            countries_raw = split_csv(spec.get("country", ""))
            currencies_raw = split_csv(spec.get("currency", ""))
            # Always remember discovered enum members, even if we collapse the row.
            acc.countries.update(countries_raw)
            acc.currencies.update(currencies_raw)
            label = f"pm_filters.{connector}.{pm}"
            countries, currencies = _maybe_collapse(
                countries_raw, currencies_raw, label=label, acc=acc,
            )

            base_ctx = {"payment_method": pm}
            if not is_default:
                base_ctx["connector"] = connector

            for frag in expand_country_currency(countries, currencies):
                acc.add_override({**base_ctx, **frag}, enabled=True)

            not_avail = spec.get("not_available_flows") or {}
            if isinstance(not_avail, dict) and not_avail.get("capture_method") == "manual":
                acc.add_override({**base_ctx, "capture_method": "manual"}, enabled=False)


@section("mandates")
def convert_mandates(source: dict, acc: Accumulator) -> None:
    """[mandates.supported_payment_methods] and [zero_mandates.*] -> overrides
    with payment_type in {mandate, zero_dollar_mandate} plus their denial
    floors."""
    acc.add_override({"payment_type": "mandate"}, enabled=False)
    acc.add_override({"payment_type": "zero_dollar_mandate"}, enabled=False)

    convert_mandate_tree(
        source.get("mandates", {}).get("supported_payment_methods", {}),
        payment_type="mandate", acc=acc,
    )
    convert_mandate_tree(
        source.get("zero_mandates", {}).get("supported_payment_methods", {}),
        payment_type="zero_dollar_mandate", acc=acc,
    )


def convert_mandate_tree(tree: dict, *, payment_type: str, acc: Accumulator) -> None:
    """Walk arbitrarily-nested mandate dicts (card.credit.connector_list or
    wallet.apple_pay = { connector_list = ... }) and emit per-pair enables."""
    def walk(node: Any, path: "list[str]") -> None:
        if not isinstance(node, dict):
            return
        if "connector_list" in node and isinstance(node["connector_list"], str):
            pm = path[-1]  # leaf parent is the payment method name
            acc.payment_methods.add(pm)
            for conn in split_csv(node["connector_list"]):
                acc.connectors.add(conn)
                acc.add_override(
                    {"connector": conn, "payment_method": pm, "payment_type": payment_type},
                    enabled=True,
                )
        for k, v in node.items():
            if k == "connector_list":
                continue
            walk(v, path + [k])
    walk(tree, [])


@section("payout_method_filters")
def convert_payouts(source: dict, acc: Accumulator) -> None:
    """[payout_method_filters.<connector>] - parallel to pm_filters but emits
    payment_type = "payout"."""
    acc.add_override({"payment_type": "payout"}, enabled=False)
    for connector, rows in source.get("payout_method_filters", {}).items():
        if not isinstance(rows, dict):
            continue
        acc.connectors.add(connector)
        for pm, spec in rows.items():
            acc.payment_methods.add(pm)
            if not isinstance(spec, dict):
                continue
            countries_raw = split_csv(spec.get("country", ""))
            currencies_raw = split_csv(spec.get("currency", ""))
            acc.countries.update(countries_raw)
            acc.currencies.update(currencies_raw)
            label = f"payout_method_filters.{connector}.{pm}"
            countries, currencies = _maybe_collapse(
                countries_raw, currencies_raw, label=label, acc=acc,
            )
            for frag in expand_country_currency(countries, currencies):
                acc.add_override(
                    {"connector": connector, "payment_method": pm,
                     "payment_type": "payout", **frag},
                    enabled=True,
                )


@section("bank_config")
def convert_bank_config(source: dict, acc: Accumulator) -> None:
    """[bank_config.<payment_method>.<connector>] = { banks = "a,b,c" } -
    materialise as an array config under `supported_banks`."""
    acc.add_config(
        "supported_banks",
        value=[],
        schema={"type": "array", "items": {"type": "string"}, "uniqueItems": True},
    )
    for pm, by_connector in source.get("bank_config", {}).items():
        if not isinstance(by_connector, dict):
            continue
        acc.payment_methods.add(pm)
        for connector, spec in by_connector.items():
            acc.connectors.add(connector)
            if not isinstance(spec, dict):
                continue
            banks = split_csv(spec.get("banks", ""))
            if not banks:
                continue
            acc.add_override(
                {"connector": connector, "payment_method": pm},
                supported_banks=banks,
            )


@section("connectors_base_url")
def convert_connectors_base_url(source: dict, acc: Accumulator) -> None:
    """[connectors] adyen.base_url = "..." - emits `base_url` per connector."""
    acc.add_config(
        "base_url",
        value="https://example.invalid/",
        schema={"type": "string", "format": "uri"},
    )
    for connector, spec in source.get("connectors", {}).items():
        if not isinstance(spec, dict):
            continue
        base_url = spec.get("base_url")
        if not base_url:
            continue
        if "{" in base_url or "$" in base_url:
            # Templated URLs need merchant-specific substitution at runtime.
            continue
        if not (base_url.startswith("http://") or base_url.startswith("https://")):
            # Source has the occasional placeholder like sanlam.base_url = "dev"
            # which fails the JSON Schema `format: uri` check.
            continue
        acc.connectors.add(connector)
        acc.add_override({"connector": connector}, base_url=base_url)


@section("connector_flags")
def convert_connector_flags(source: dict, acc: Accumulator) -> None:
    """The per-connector CSV "connector_list" sections each get their own
    boolean flag config keyed on `connector`."""
    flags = [
        # (source_section, source_key, output_config_key)
        ("network_transaction_id_supported_connectors", "connector_list",
         "network_transaction_id_supported"),
        ("card_only_mit_supported_connectors", "connector_list",
         "card_only_mit_supported"),
        ("notify_iframe_exit_and_redirect", "connector_list",
         "notify_iframe_exit_and_redirect"),
        ("delayed_session_response", "connectors_with_delayed_session_response",
         "delayed_session_response"),
        ("webhook_source_verification_call",
         "connectors_with_webhook_source_verification_call",
         "webhook_source_verification_required"),
        ("billing_connectors_payment_sync",
         "billing_connectors_which_require_payment_sync",
         "billing_payment_sync_required"),
        ("billing_connectors_invoice_sync",
         "billing_connectors_which_requires_invoice_sync_call",
         "billing_invoice_sync_required"),
        ("connector_customer", "payout_connector_list",
         "payout_customer_connector_supported"),
    ]
    for src_section, src_key, out_key in flags:
        section_data = source.get(src_section, {})
        if not isinstance(section_data, dict):
            continue
        connector_list = split_csv(section_data.get(src_key, ""))
        if not connector_list:
            continue
        acc.add_config(out_key, value=False, schema={"type": "boolean"})
        for conn in connector_list:
            acc.connectors.add(conn)
            acc.add_override({"connector": conn}, **{out_key: True})


@section("tokenization")
def convert_tokenization(source: dict, acc: Accumulator) -> None:
    """[tokenization] <connector> = { long_lived_token = ..., payment_method = "..." }"""
    tok = source.get("tokenization", {})
    if not isinstance(tok, dict):
        return
    acc.add_config("tokenization_enabled", value=False, schema={"type": "boolean"})
    acc.add_config("tokenization_long_lived_token", value=False, schema={"type": "boolean"})
    for connector, spec in tok.items():
        if not isinstance(spec, dict):
            continue
        acc.connectors.add(connector)
        pms_raw = split_csv(spec.get("payment_method", ""))
        # Expand umbrellas to concrete payment methods we model.
        pms: "list[str]" = []
        for p in pms_raw:
            pms.extend(PM_UMBRELLA.get(p, [p]))
        pms = sorted(set(pms))
        long_lived = bool(spec.get("long_lived_token", False))
        for pm in pms:
            acc.payment_methods.add(pm)
            acc.add_override(
                {"connector": connector, "payment_method": pm},
                tokenization_enabled=True,
                tokenization_long_lived_token=long_lived,
            )


@section("temp_locker")
def convert_temp_locker(source: dict, acc: Accumulator) -> None:
    """[temp_locker_enable_config] <connector> = { payment_method = "card" }"""
    cfg = source.get("temp_locker_enable_config", {})
    if not isinstance(cfg, dict):
        return
    acc.add_config("temp_locker_enabled", value=False, schema={"type": "boolean"})
    for connector, spec in cfg.items():
        if not isinstance(spec, dict):
            continue
        acc.connectors.add(connector)
        pms_raw = split_csv(spec.get("payment_method", ""))
        pms: "list[str]" = []
        for p in pms_raw:
            pms.extend(PM_UMBRELLA.get(p, [p]))
        for pm in sorted(set(pms)):
            acc.payment_methods.add(pm)
            acc.add_override(
                {"connector": connector, "payment_method": pm},
                temp_locker_enabled=True,
            )


# ---------------------------------------------------------------------------
# Emit SuperTOML
# ---------------------------------------------------------------------------

def emit_supertoml(acc: Accumulator) -> str:
    """Render the accumulated state as SuperTOML, deterministically."""
    lines: "list[str]" = []

    lines.append("# Generated from Hyperswitch development.toml by")
    lines.append("# generate_hyperswitch_supertoml.py - DO NOT EDIT BY HAND.")
    lines.append("#")
    lines.append(f"# Sections converted: {', '.join(acc.sections_run)}")
    lines.append("")

    # default-configs
    lines.append("[default-configs]")
    lines.append(
        f"enabled = {toml_value({'value': False, 'schema': {'type': 'boolean'}})}"
    )
    for key in sorted(acc.extra_configs.keys()):
        lines.append(f"{key} = {toml_value(acc.extra_configs[key])}")
    lines.append("")

    # dimensions
    lines.append("[dimensions]")
    lines.append(
        f"connector = {toml_value({'position': POSITIONS['connector'], 'schema': {'type': 'string', 'enum': sorted(acc.connectors)}})}"
    )
    lines.append(
        f"country = {toml_value({'position': POSITIONS['country'], 'schema': {'type': 'string', 'enum': sorted(acc.countries)}})}"
    )
    lines.append(
        f"payment_method = {toml_value({'position': POSITIONS['payment_method'], 'schema': {'type': 'string', 'enum': sorted(acc.payment_methods)}})}"
    )
    lines.append(
        f"currency = {toml_value({'position': POSITIONS['currency'], 'schema': {'type': 'string', 'enum': sorted(acc.currencies)}})}"
    )
    lines.append(
        f"capture_method = {toml_value({'position': POSITIONS['capture_method'], 'schema': {'type': 'string', 'enum': ['automatic', 'manual']}})}"
    )
    lines.append(
        f"payment_type = {toml_value({'position': POSITIONS['payment_type'], 'schema': {'type': 'string', 'enum': ['normal', 'mandate', 'zero_dollar_mandate', 'payout']}})}"
    )
    # region cohort
    region_enum = list(REGIONS.keys()) + ["otherwise"]
    region_defs = {r: {"in": [{"var": "country"}, cs]} for r, cs in REGIONS.items()}
    lines.append(
        f"region = {toml_value({'position': POSITIONS['region'], 'type': 'LOCAL_COHORT:country', 'schema': {'type': 'string', 'enum': region_enum, 'definitions': region_defs}})}"
    )
    lines.append("")

    # overrides - sorted for determinism. Order by (priority desc, context json).
    def override_sort_key(ov: dict) -> tuple:
        ctx = ov["_context_"]
        priority = sum(2 ** POSITIONS.get(k, 0) for k in ctx)
        return (-priority, json.dumps(ctx, sort_keys=True),
                json.dumps({k: v for k, v in ov.items() if k != "_context_"}, sort_keys=True))

    for ov in sorted(acc.overrides, key=override_sort_key):
        lines.append("[[overrides]]")
        lines.append(f"_context_ = {toml_value(ov['_context_'])}")
        for k, v in ov.items():
            if k == "_context_":
                continue
            lines.append(f"{k} = {toml_value(v)}")
        lines.append("")

    return "\n".join(lines).rstrip() + "\n"


# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

@dataclass
class ResolverConfig:
    default_configs: dict
    dimensions: dict
    overrides: "list[dict]"
    # Populated only when bindings are in use; opaque Config struct from the
    # native lib, kept so we can call ffi_eval_config without re-parsing.
    bindings_config: Any = None


def _augment_with_cohorts(query: dict) -> dict:
    """Mirror Superposition's evaluate_local_cohorts: add `region` derived
    from `country` if present."""
    q = dict(query)
    country = q.get("country")
    if country and "region" not in q:
        q["region"] = REGION_OF.get(country, "otherwise")
    return q


def _resolve_fallback(cfg: ResolverConfig, query: dict) -> dict:
    """Python-only resolver: stand-in for ffi_eval_config when the native lib
    isn't installed. Implements the same priority sum + cohort derivation."""
    query = _augment_with_cohorts(query)
    result = {k: spec["value"] for k, spec in cfg.default_configs.items()}
    matching: "list[tuple[int, dict]]" = []
    for ov in cfg.overrides:
        ctx = ov["_context_"]
        if all(query.get(k) == v for k, v in ctx.items()):
            priority = sum(2 ** POSITIONS.get(k, 0) for k in ctx)
            matching.append((priority, ov))
    matching.sort(key=lambda x: x[0])
    for _, ov in matching:
        for k, v in ov.items():
            if k == "_context_":
                continue
            result[k] = v
    return result


def _resolve_bindings(cfg: ResolverConfig, query: dict) -> dict:
    """Authoritative resolver: routes through ffi_eval_config which is the
    same Rust code superposition_core uses in production."""
    # Native lib expects dict[str, JSON-encoded value] for both query and
    # result. Wrap on entry, unwrap on exit.
    encoded_query = {k: json.dumps(v) for k, v in query.items()}
    raw = _ffi_eval_config(
        cfg.bindings_config.default_configs,
        cfg.bindings_config.contexts,
        cfg.bindings_config.overrides,
        cfg.bindings_config.dimensions,
        encoded_query,
        _BindingsMergeStrategy.MERGE,
        None,
        None,
    )
    return {k: json.loads(v) for k, v in raw.items()}


def resolve(cfg: ResolverConfig, query: dict) -> dict:
    if cfg.bindings_config is not None:
        return _resolve_bindings(cfg, query)
    return _resolve_fallback(cfg, query)


def parse_supertoml(text: str) -> ResolverConfig:
    """Parse for validation. Always runs the Python TOML parse (for schema
    checks against the in-Python override shape), and additionally invokes
    ffi_parse_toml_config when bindings are available so the JSON Schema
    validation and other Rust-side checks run."""
    parsed = tomllib.loads(text)
    bindings_cfg = None
    if BINDINGS_AVAILABLE:
        # This call enforces JSON Schema validation on default_configs and
        # raises if the file isn't a valid SuperTOML (not merely valid TOML).
        bindings_cfg = _ffi_parse_toml_config(text)
    return ResolverConfig(
        default_configs=parsed.get("default-configs", {}),
        dimensions=parsed.get("dimensions", {}),
        overrides=parsed.get("overrides", []),
        bindings_config=bindings_cfg,
    )


def schema_check(cfg: ResolverConfig) -> "list[str]":
    errors: "list[str]" = []
    valid_dims = set(cfg.dimensions.keys())
    valid_configs = set(cfg.default_configs.keys())
    enum_values = {
        d: set(info.get("schema", {}).get("enum", []))
        for d, info in cfg.dimensions.items()
    }
    for i, ov in enumerate(cfg.overrides):
        ctx = ov.get("_context_", {})
        for k, v in ctx.items():
            if k not in valid_dims:
                errors.append(f"override #{i}: undeclared dimension {k!r}")
            elif enum_values.get(k) and v not in enum_values[k]:
                errors.append(f"override #{i}: {k}={v!r} not in declared enum")
        for k in ov:
            if k == "_context_":
                continue
            if k not in valid_configs:
                errors.append(f"override #{i}: undeclared config key {k!r}")
    return errors


# Parity matrix: (name, requires_section, query, expected_subset). Tests are
# skipped (not failed) when their required section is absent - that's what
# makes phased generation runs validate cleanly.
PARITY_TESTS: "list[tuple[str, str, dict, dict]]" = [
    ("affirm US/USD normal", "pm_filters", {"payment_method": "affirm", "country": "US", "currency": "USD", "payment_type": "normal"}, {"enabled": True}),
    # Source enables sepa only per-connector ([pm_filters.stripe], [pm_filters.adyen]);
    # there is no sepa entry in [pm_filters.default].
    ("sepa DE/EUR via stripe", "pm_filters", {"connector": "stripe", "payment_method": "sepa", "country": "DE", "currency": "EUR", "payment_type": "normal"}, {"enabled": True}),
    ("sepa BR/BRL no connector (default has no sepa)", "pm_filters", {"payment_method": "sepa", "country": "BR", "currency": "BRL", "payment_type": "normal"}, {"enabled": False}),
    ("ideal NL adyen automatic", "pm_filters", {"connector": "adyen", "payment_method": "ideal", "country": "NL", "currency": "EUR", "capture_method": "automatic", "payment_type": "normal"}, {"enabled": True}),
    ("ideal NL adyen manual", "pm_filters", {"connector": "adyen", "payment_method": "ideal", "country": "NL", "currency": "EUR", "capture_method": "manual", "payment_type": "normal"}, {"enabled": False}),
    ("ideal NL stripe manual (no source gate)", "pm_filters", {"connector": "stripe", "payment_method": "ideal", "country": "NL", "currency": "EUR", "capture_method": "manual", "payment_type": "normal"}, {"enabled": True}),
    ("stripe credit mandate", "mandates", {"connector": "stripe", "payment_method": "credit", "payment_type": "mandate"}, {"enabled": True}),
    # Stripe IS in card.credit.connector_list under [zero_mandates] (line 1176)
    # - the hand-written conversion missed this; the generator gets it right.
    ("stripe credit zero_dollar_mandate", "mandates", {"connector": "stripe", "payment_method": "credit", "payment_type": "zero_dollar_mandate"}, {"enabled": True}),
    ("hipay credit zero_dollar_mandate (not in source)", "mandates", {"connector": "hipay", "payment_method": "credit", "payment_type": "zero_dollar_mandate"}, {"enabled": False}),
    ("adyen credit mandate", "mandates", {"connector": "adyen", "payment_method": "credit", "payment_type": "mandate"}, {"enabled": True}),
    ("adyen credit zero_dollar_mandate", "mandates", {"connector": "adyen", "payment_method": "credit", "payment_type": "zero_dollar_mandate"}, {"enabled": True}),
    ("gocardless ach mandate", "mandates", {"connector": "gocardless", "payment_method": "ach", "payment_type": "mandate"}, {"enabled": True}),
    ("hipay credit mandate (NOT in source)", "mandates", {"connector": "hipay", "payment_method": "credit", "payment_type": "mandate"}, {"enabled": False}),
    ("stripe ach US/USD payout", "payout_method_filters", {"connector": "stripe", "payment_method": "ach", "country": "US", "currency": "USD", "payment_type": "payout"}, {"enabled": True}),
    ("stripe credit payout (not configured)", "payout_method_filters", {"connector": "stripe", "payment_method": "credit", "payment_type": "payout"}, {"enabled": False}),
    ("truelayer open_banking GBP payout", "payout_method_filters", {"connector": "truelayer", "payment_method": "open_banking", "currency": "GBP", "payment_type": "payout"}, {"enabled": True}),
    ("loonio interac CAD payout", "payout_method_filters", {"connector": "loonio", "payment_method": "interac", "currency": "CAD", "payment_type": "payout"}, {"enabled": True}),
    ("adyen base_url", "connectors_base_url", {"connector": "adyen"}, {"base_url": "https://checkout-test.adyen.com/"}),
    ("stripe NTID supported", "connector_flags", {"connector": "stripe"}, {"network_transaction_id_supported": True}),
    ("hipay NTID NOT supported", "connector_flags", {"connector": "hipay"}, {"network_transaction_id_supported": False}),
    ("worldpayxml iframe redirect", "connector_flags", {"connector": "worldpayxml"}, {"notify_iframe_exit_and_redirect": True}),
    ("trustpay delayed session", "connector_flags", {"connector": "trustpay"}, {"delayed_session_response": True}),
    ("paypal webhook verification", "connector_flags", {"connector": "paypal"}, {"webhook_source_verification_required": True}),
    ("recurly invoice_sync", "connector_flags", {"connector": "recurly"}, {"billing_invoice_sync_required": True}),
    ("stripebilling invoice_sync (NOT in source)", "connector_flags", {"connector": "stripebilling"}, {"billing_invoice_sync_required": False}),
    ("adyen iDEAL banks (NL)", "bank_config", {"connector": "adyen", "payment_method": "ideal", "country": "NL"}, {"supported_banks_includes": "abn_amro"}),
    ("stripe bank_transfer temp_locker", "temp_locker", {"connector": "stripe", "payment_method": "bank_transfer"}, {"temp_locker_enabled": True}),
    ("stripe credit temp_locker (NOT in source)", "temp_locker", {"connector": "stripe", "payment_method": "credit"}, {"temp_locker_enabled": False}),
]


def parity_check(cfg: ResolverConfig, sections_run: "set[str]") -> "tuple[int, int, list[str]]":
    """Returns (ran, skipped, failures). Tests requiring a section that
    wasn't converted are skipped instead of failed."""
    failures: "list[str]" = []
    ran = 0
    skipped = 0
    for name, req_section, query, expected in PARITY_TESTS:
        if req_section not in sections_run:
            skipped += 1
            continue
        ran += 1
        result = resolve(cfg, query)
        for key, want in expected.items():
            if key == "supported_banks_includes":
                if "supported_banks" not in cfg.default_configs:
                    continue
                got = result.get("supported_banks", [])
                if want not in got:
                    failures.append(f"{name}: supported_banks missing {want!r} (got {got!r})")
            else:
                if key not in cfg.default_configs:
                    continue
                got = result.get(key)
                if got != want:
                    failures.append(f"{name}: expected {key}={want!r}, got {got!r}")
    return ran, skipped, failures


# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------

DEFAULT_SOURCE = "/tmp/hyperswitch-dev.toml"
# Defaults to a sibling filename so we don't clobber the hand-curated
# hyperswitch-development.toml on first run. During the phased migration the
# two files can be diffed; once parity is reached, replace and delete this one.
DEFAULT_OUTPUT = str(Path(__file__).parent / "hyperswitch-development.generated.toml")


def main() -> int:
    p = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--source", default=DEFAULT_SOURCE,
                   help=f"path to Hyperswitch development.toml (default: {DEFAULT_SOURCE})")
    p.add_argument("--output", default=DEFAULT_OUTPUT,
                   help=f"path to write SuperTOML to (default: alongside this script)")
    p.add_argument("--sections", default=",".join(DEFAULT_SECTIONS),
                   help=f"comma-separated subset of sections to convert. "
                        f"Available: {', '.join(SECTIONS.keys())}")
    p.add_argument("--validate-only", action="store_true",
                   help="don't rewrite output; just validate it")
    p.add_argument("--no-bindings", action="store_true",
                   help="force the pure-Python fallback resolver even if "
                        "superposition_bindings is installed (for testing)")
    args = p.parse_args()

    global BINDINGS_AVAILABLE
    if args.no_bindings and BINDINGS_AVAILABLE:
        BINDINGS_AVAILABLE = False

    requested = [s.strip() for s in args.sections.split(",") if s.strip()]
    unknown = [s for s in requested if s not in SECTIONS]
    if unknown:
        print(f"ERROR: unknown section(s): {', '.join(unknown)}", file=sys.stderr)
        print(f"Available: {', '.join(SECTIONS.keys())}", file=sys.stderr)
        return 2

    src_path = Path(args.source)
    out_path = Path(args.output)

    if not args.validate_only:
        if not src_path.exists():
            print(f"ERROR: source file not found: {src_path}", file=sys.stderr)
            return 2
        with src_path.open("rb") as fh:
            source = tomllib.load(fh)

        acc = Accumulator()
        acc.sections_run = requested
        for name in requested:
            SECTIONS[name](source, acc)

        rendered = emit_supertoml(acc)
        out_path.write_text(rendered)
        print(f"Wrote {out_path}")
        print(f"  default-configs: {1 + len(acc.extra_configs)}")
        print(f"  dimensions:      7")
        print(f"  overrides:       {len(acc.overrides)}")
        print(f"  connectors enum: {len(acc.connectors)}")
        print(f"  payment_methods: {len(acc.payment_methods)}")
        print(f"  countries:       {len(acc.countries)}")
        print(f"  currencies:      {len(acc.currencies)}")
        if acc.collapsed_rows:
            print(f"\n  Collapsed {len(acc.collapsed_rows)} rows past threshold "
                  f"(COUNTRY={COUNTRY_THRESHOLD}, CURRENCY={CURRENCY_THRESHOLD}):")
            for row in acc.collapsed_rows[:8]:
                print(f"    - {row}")
            if len(acc.collapsed_rows) > 8:
                print(f"    ... and {len(acc.collapsed_rows) - 8} more "
                      f"(these are the candidates for per-row refinement in a later phase)")

    # Validate.
    if not out_path.exists():
        print(f"ERROR: no SuperTOML at {out_path} to validate", file=sys.stderr)
        return 2
    text = out_path.read_text()
    try:
        cfg = parse_supertoml(text)
    except tomllib.TOMLDecodeError as e:
        print(f"VALIDATION FAILED: TOML parse error: {e}", file=sys.stderr)
        return 1

    print("\nValidation:")
    if BINDINGS_AVAILABLE:
        print(f"  Resolver: superposition_bindings (Rust-backed, authoritative)")
    else:
        msg = "fallback (--no-bindings)" if args.no_bindings else (
            f"fallback - bindings unavailable: {BINDINGS_LOAD_ERROR}"
        )
        print(f"  Resolver: pure-Python {msg}")
    print(f"  Parsed OK ({len(cfg.default_configs)} configs, "
          f"{len(cfg.dimensions)} dimensions, {len(cfg.overrides)} overrides)")

    schema_errors = schema_check(cfg)
    if schema_errors:
        print("  Schema errors:")
        for e in schema_errors[:10]:
            print(f"    - {e}")
        if len(schema_errors) > 10:
            print(f"    ... and {len(schema_errors) - 10} more")
        return 1
    print(f"  Schema check passed (every override references declared dims and configs)")

    # Recover the sections list from the generated header so --validate-only
    # also knows which parity tests apply.
    if args.validate_only:
        sections_run = set()
        for line in text.splitlines()[:10]:
            if line.startswith("# Sections converted:"):
                sections_run = {s.strip() for s in line.split(":", 1)[1].split(",")}
                break
        if not sections_run:
            print("  WARN: could not find sections header; assuming all sections present")
            sections_run = set(DEFAULT_SECTIONS)
    else:
        sections_run = set(requested)

    ran, skipped, failures = parity_check(cfg, sections_run)
    if failures:
        print(f"  Parity: {ran - len(failures)}/{ran} pass ({skipped} skipped for absent sections)")
        for f in failures:
            print(f"    - {f}")
        return 1
    print(f"  Parity: {ran}/{ran} pass ({skipped} skipped for absent sections)")
    print("OK")
    return 0


if __name__ == "__main__":
    sys.exit(main())
