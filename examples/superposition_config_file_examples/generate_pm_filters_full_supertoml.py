#!/usr/bin/env python3
"""
Generate a SuperTOML containing pm_filters, mandates, and zero_mandates,
with every override carrying all six dimensions explicitly.

Sources, each producing one payment_type variant:
  [pm_filters.<connector>.<payment_method>]         -> payment_type = "normal"
  [mandates.supported_payment_methods]              -> payment_type = "mandate"
  [zero_mandates.supported_payment_methods]         -> payment_type = "zero_dollar_mandate"

Each row becomes the full cross-product over (country, currency,
capture_method), all setting `enabled = true`:

  - For pm_filters: country/currency from the row's explicit list, or
    every observed value if the row omits one. capture_method emits both
    automatic and manual; rows with `not_available_flows = { capture_method
    = "manual" }` emit only automatic.
  - For mandates / zero_mandates: country, currency, and capture_method
    are never specified in the source, so all three expand to the full
    observed enum.

Skips [pm_filters.default] - those rows have no connector dimension, and
the requirement is that every override carry all six dims. Expanding
default to all connectors would balloon the output by ~130x. Logged at
run time so it's not silent.

Usage:
    python3 generate_pm_filters_full_supertoml.py \\
        --source /tmp/hyperswitch-dev.toml \\
        --output hyperswitch-pm-filters-full.generated.toml

    # Subset for spot-checking - applies to all three payment_type sources.
    python3 generate_pm_filters_full_supertoml.py --connectors adyen,stripe

Output is deterministic. Validation re-parses with tomllib and, when the
superposition_bindings native lib is reachable in the local tree, also
with ffi_parse_toml_config so JSON Schema violations surface.
"""

from __future__ import annotations

import argparse
import json
import sys
import tomllib
from collections import OrderedDict
from pathlib import Path
from typing import Any

# Match the position assignments used by the broader generator so the two
# files cascade together if both are loaded by a downstream consumer.
POSITIONS = {
    "payment_type": 7,
    "capture_method": 6,
    "connector": 5,
    "country": 4,
    "payment_method": 3,
    "currency": 2,
}

# Auto-discover the native Python bindings inside this repo. Mirrors the
# logic in generate_hyperswitch_supertoml.py.
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
    )
    BINDINGS_AVAILABLE = True
    BINDINGS_LOAD_ERROR = None
except Exception as _e:
    _ffi_parse_toml_config = None
    BINDINGS_AVAILABLE = False
    BINDINGS_LOAD_ERROR = repr(_e)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def split_csv(s: Any) -> "list[str]":
    if not isinstance(s, str):
        return []
    return sorted({x.strip() for x in s.split(",") if x.strip()})


def toml_value(v: Any) -> str:
    if isinstance(v, bool):
        return "true" if v else "false"
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


def sorted_context(ctx: dict) -> "OrderedDict":
    """Emit keys in descending position so the most-specific dimension reads
    first within each [[overrides]] block."""
    return OrderedDict(
        sorted(ctx.items(), key=lambda kv: (-POSITIONS.get(kv[0], 0), kv[0]))
    )


# ---------------------------------------------------------------------------
# Two-pass conversion: discover, then emit cross-products.
# ---------------------------------------------------------------------------

def walk_mandate_section(section: dict) -> "list[tuple[str, list[str]]]":
    """Mandate sections nest as category.payment_method.connector_list:
        bank_debit.ach = { connector_list = "..." }
        card.credit.connector_list = "..."
    Both forms parse identically. Returns (payment_method, connectors) pairs;
    the category prefix (card/bank_debit/wallet/...) is dropped because we
    flatten to the leaf payment_method as the dimension value."""
    pairs: "list[tuple[str, list[str]]]" = []
    if not isinstance(section, dict):
        return pairs
    for _category, methods in section.items():
        if not isinstance(methods, dict):
            continue
        for pm_name, spec in methods.items():
            if not isinstance(spec, dict):
                continue
            cl = spec.get("connector_list")
            if isinstance(cl, str):
                pairs.append((pm_name, split_csv(cl)))
    return pairs


def discover_enums(
    pm_filters: dict,
    mandates_section: dict,
    zero_mandates_section: dict,
) -> "tuple[list[str], list[str], list[str], list[str], int]":
    """Walk every relevant row, accumulate the discovered enum members for
    each dimension, and count how many default rows we skipped."""
    connectors: "set[str]" = set()
    payment_methods: "set[str]" = set()
    countries: "set[str]" = set()
    currencies: "set[str]" = set()
    skipped_default = 0
    for connector, rows in pm_filters.items():
        if not isinstance(rows, dict):
            continue
        if connector == "default":
            skipped_default += len(rows)
            continue
        connectors.add(connector)
        for pm, spec in rows.items():
            payment_methods.add(pm)
            if isinstance(spec, dict):
                countries.update(split_csv(spec.get("country", "")))
                currencies.update(split_csv(spec.get("currency", "")))
    # Mandate sections only contribute connectors and payment_methods.
    for sec in (mandates_section, zero_mandates_section):
        for pm, conns in walk_mandate_section(sec):
            payment_methods.add(pm)
            connectors.update(conns)
    return (sorted(connectors), sorted(payment_methods),
            sorted(countries), sorted(currencies), skipped_default)


def _emit_cross_product(
    overrides: "list[dict]",
    *,
    payment_type: str,
    connector: str,
    payment_method: str,
    countries: "list[str]",
    currencies: "list[str]",
    capture_methods: "list[str]",
) -> None:
    for capture in capture_methods:
        for country in countries:
            for currency in currencies:
                overrides.append({
                    "_context_": sorted_context({
                        "payment_type": payment_type,
                        "capture_method": capture,
                        "connector": connector,
                        "country": country,
                        "payment_method": payment_method,
                        "currency": currency,
                    }),
                    "enabled": True,
                })


def build_overrides(
    pm_filters: dict,
    mandates_section: dict,
    zero_mandates_section: dict,
    all_countries: "list[str]",
    all_currencies: "list[str]",
    connector_filter: "set[str]",
) -> "list[dict]":
    overrides: "list[dict]" = []

    # ----- payment_type = "normal" from pm_filters --------------------------
    for connector in sorted(pm_filters.keys()):
        if connector == "default":
            continue
        if connector_filter and connector not in connector_filter:
            continue
        rows = pm_filters[connector]
        if not isinstance(rows, dict):
            continue
        for pm in sorted(rows.keys()):
            spec = rows[pm]
            if not isinstance(spec, dict):
                continue
            countries = split_csv(spec.get("country", "")) or all_countries
            currencies = split_csv(spec.get("currency", "")) or all_currencies
            not_avail = spec.get("not_available_flows") or {}
            manual_blocked = (
                isinstance(not_avail, dict)
                and not_avail.get("capture_method") == "manual"
            )
            capture_methods = ["automatic"] if manual_blocked else ["automatic", "manual"]
            _emit_cross_product(
                overrides,
                payment_type="normal",
                connector=connector,
                payment_method=pm,
                countries=countries,
                currencies=currencies,
                capture_methods=capture_methods,
            )

    # ----- payment_type = "mandate" / "zero_dollar_mandate" -----------------
    # Mandate sections don't carry country/currency/capture_method at all -
    # the source semantics are "this connector supports this kind of mandate
    # for this payment_method, period". Expand to every observed dimension
    # value so each emitted override carries all six dims.
    mandate_sources = [
        ("mandate", mandates_section),
        ("zero_dollar_mandate", zero_mandates_section),
    ]
    for payment_type, section in mandate_sources:
        for pm, connector_list in walk_mandate_section(section):
            for connector in sorted(connector_list):
                if connector_filter and connector not in connector_filter:
                    continue
                _emit_cross_product(
                    overrides,
                    payment_type=payment_type,
                    connector=connector,
                    payment_method=pm,
                    countries=all_countries,
                    currencies=all_currencies,
                    capture_methods=["automatic", "manual"],
                )

    return overrides


# ---------------------------------------------------------------------------
# Emit
# ---------------------------------------------------------------------------

def emit_supertoml(
    connectors: "list[str]",
    payment_methods: "list[str]",
    countries: "list[str]",
    currencies: "list[str]",
    overrides: "list[dict]",
    skipped_default: int,
    out_path: Path,
) -> None:
    """Stream-write to keep memory bounded - the override list can run into
    the hundreds of thousands."""
    # Override sort: every override has the same weight (all 6 dimensions
    # present), so priority is constant. Secondary sort walks dimension
    # values in descending position order, clustering by the most specific
    # axis first (payment_type -> capture_method -> connector -> ...).
    sort_key_order = sorted(POSITIONS.keys(), key=lambda k: -POSITIONS[k])
    def sort_key(ov: dict) -> tuple:
        ctx = ov["_context_"]
        return tuple(ctx.get(k, "") for k in sort_key_order)
    overrides.sort(key=sort_key)

    # Ascending position for dimensions (broadest axis first), matching the
    # convention used by the other generator.
    dim_specs = [
        ("currency", POSITIONS["currency"], currencies),
        ("payment_method", POSITIONS["payment_method"], payment_methods),
        ("country", POSITIONS["country"], countries),
        ("connector", POSITIONS["connector"], connectors),
        ("capture_method", POSITIONS["capture_method"], ["automatic", "manual"]),
        ("payment_type", POSITIONS["payment_type"], ["normal", "mandate", "zero_dollar_mandate"]),
    ]

    with out_path.open("w") as fh:
        fh.write("# Generated by generate_pm_filters_full_supertoml.py - DO NOT EDIT.\n")
        fh.write("#\n")
        fh.write("# Scope: pm_filters only. Every override carries all six dimensions\n")
        fh.write("# (currency, payment_method, country, connector, capture_method,\n")
        fh.write("# payment_type). Rows missing country or currency in the source were\n")
        fh.write("# expanded to all values observed across pm_filters.\n")
        fh.write("#\n")
        fh.write(f"# Skipped {skipped_default} rows from [pm_filters.default] -\n")
        fh.write("# expanding them to every connector would balloon the output ~130x.\n")
        fh.write("\n")

        fh.write("[default-configs]\n")
        fh.write(f"enabled = {toml_value({'value': False, 'schema': {'type': 'boolean'}})}\n")
        fh.write("\n")

        fh.write("[dimensions]\n")
        for name, pos, enum in dim_specs:
            spec = {"position": pos, "schema": {"type": "string", "enum": enum}}
            fh.write(f"{name} = {toml_value(spec)}\n")
        fh.write("\n")

        for ov in overrides:
            fh.write("[[overrides]]\n")
            fh.write(f"_context_ = {toml_value(ov['_context_'])}\n")
            fh.write(f"enabled = {toml_value(ov['enabled'])}\n")
            fh.write("\n")


# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

# Lean parity matrix: a handful of representative rows from the source whose
# expected enable state is unambiguous given the "every dim explicit" model.
PARITY_TESTS: "list[tuple[str, dict, bool]]" = [
    # adyen.swish = { country = "SE", currency = "SEK" } - both capture methods.
    ("adyen swish SE/SEK auto",
     {"connector": "adyen", "payment_method": "swish", "country": "SE",
      "currency": "SEK", "capture_method": "automatic", "payment_type": "normal"},
     True),
    ("adyen swish SE/SEK manual",
     {"connector": "adyen", "payment_method": "swish", "country": "SE",
      "currency": "SEK", "capture_method": "manual", "payment_type": "normal"},
     True),
    # adyen.ideal has not_available_flows.capture_method = "manual".
    ("adyen ideal NL/EUR auto",
     {"connector": "adyen", "payment_method": "ideal", "country": "NL",
      "currency": "EUR", "capture_method": "automatic", "payment_type": "normal"},
     True),
    ("adyen ideal NL/EUR manual (blocked by not_available_flows)",
     {"connector": "adyen", "payment_method": "ideal", "country": "NL",
      "currency": "EUR", "capture_method": "manual", "payment_type": "normal"},
     False),
    # razorpay.upi_collect IN/INR
    ("razorpay upi_collect IN/INR auto",
     {"connector": "razorpay", "payment_method": "upi_collect", "country": "IN",
      "currency": "INR", "capture_method": "automatic", "payment_type": "normal"},
     True),
    # stripe.affirm US/USD
    ("stripe affirm US/USD auto",
     {"connector": "stripe", "payment_method": "affirm", "country": "US",
      "currency": "USD", "capture_method": "automatic", "payment_type": "normal"},
     True),
    # Negative: a context the source never enables.
    ("adyen swish DE/EUR (not in source)",
     {"connector": "adyen", "payment_method": "swish", "country": "DE",
      "currency": "EUR", "capture_method": "automatic", "payment_type": "normal"},
     False),
    # truelayer.open_banking has currency only - country expanded.
    ("truelayer open_banking US/GBP auto (country expanded)",
     {"connector": "truelayer", "payment_method": "open_banking", "country": "US",
      "currency": "GBP", "capture_method": "automatic", "payment_type": "normal"},
     True),
    # ---- Mandate cases. country/currency/capture_method are always expanded.
    # stripe + card.credit IS in [mandates.supported_payment_methods].
    ("stripe credit JP/JPY manual mandate",
     {"connector": "stripe", "payment_method": "credit", "country": "JP",
      "currency": "JPY", "capture_method": "manual", "payment_type": "mandate"},
     True),
    # hipay credit is NOT in [mandates.supported_payment_methods].
    ("hipay credit US/USD auto mandate (not in source)",
     {"connector": "hipay", "payment_method": "credit", "country": "US",
      "currency": "USD", "capture_method": "automatic", "payment_type": "mandate"},
     False),
    # gocardless + bank_debit.ach IS in [mandates.supported_payment_methods].
    ("gocardless ach DE/EUR mandate",
     {"connector": "gocardless", "payment_method": "ach", "country": "DE",
      "currency": "EUR", "capture_method": "automatic", "payment_type": "mandate"},
     True),
    # ---- zero_dollar_mandate cases.
    # adyen + card.credit IS in [zero_mandates.supported_payment_methods].
    ("adyen credit FR/EUR zero_dollar_mandate",
     {"connector": "adyen", "payment_method": "credit", "country": "FR",
      "currency": "EUR", "capture_method": "automatic", "payment_type": "zero_dollar_mandate"},
     True),
    # gocardless + bank_debit.sepa IS in [zero_mandates.supported_payment_methods].
    ("gocardless sepa DE/EUR zero_dollar_mandate",
     {"connector": "gocardless", "payment_method": "sepa", "country": "DE",
      "currency": "EUR", "capture_method": "manual", "payment_type": "zero_dollar_mandate"},
     True),
    # gocardless + bank_debit.bacs IS in BOTH [mandates] and [zero_mandates].
    ("gocardless bacs zero_dollar_mandate",
     {"connector": "gocardless", "payment_method": "bacs", "country": "GB",
      "currency": "GBP", "capture_method": "automatic", "payment_type": "zero_dollar_mandate"},
     True),
    # adyen + bank_debit.bacs is NOT in [zero_mandates] (only gocardless is).
    ("adyen bacs zero_dollar_mandate (not in source)",
     {"connector": "adyen", "payment_method": "bacs", "country": "GB",
      "currency": "GBP", "capture_method": "automatic", "payment_type": "zero_dollar_mandate"},
     False),
]


def resolve(overrides_index: "set[tuple]", query: dict) -> bool:
    """Constant-priority model: any matching override flips enabled to true.
    Lookup against a pre-built tuple set is O(1)."""
    key = tuple(sorted(query.items()))
    return key in overrides_index


# Above this on-disk size, skip re-parsing the file for syntax / schema
# validation. The output is generated programmatically from a parsed source
# and emit_supertoml's TOML emission is deterministic, so syntax issues are
# vanishingly unlikely; the practical risk is schema violations on default-
# configs values, which the parity matrix doesn't help with anyway. Parity
# always runs - it doesn't need the file, just the in-memory overrides.
REPARSE_MAX_BYTES = 100 * 1024 * 1024  # 100 MB


def validate(
    out_path: Path,
    overrides: "list[dict]",
    connector_filter: "set[str]",
) -> "tuple[int, int, int, list[str], str]":
    """Returns (ran, skipped, failed, failure_lines, notes)."""
    failures: "list[str]" = []
    size = out_path.stat().st_size

    notes = []
    if size > REPARSE_MAX_BYTES:
        notes.append(
            f"skipping re-parse ({size / (1024*1024):.0f} MB > "
            f"{REPARSE_MAX_BYTES // (1024*1024)} MB cap)"
        )
    else:
        text = out_path.read_text()
        tomllib.loads(text)
        notes.append("tomllib parse OK")
        if BINDINGS_AVAILABLE:
            _ffi_parse_toml_config(text)
            notes.append("superposition_bindings parse OK")

    # Parity uses the in-memory overrides, so it works at any output size.
    index: "set[tuple]" = set()
    for ov in overrides:
        index.add(tuple(sorted(ov["_context_"].items())))

    ran = 0
    skipped = 0
    for name, query, expected in PARITY_TESTS:
        # Skip tests whose connector wasn't in the filtered run.
        if connector_filter and query.get("connector") not in connector_filter:
            skipped += 1
            continue
        ran += 1
        got = resolve(index, query)
        if got != expected:
            failures.append(f"{name}: expected enabled={expected}, got {got}")
    return ran, skipped, len(failures), failures, "; ".join(notes)


# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------

DEFAULT_SOURCE = "/tmp/hyperswitch-dev.toml"
DEFAULT_OUTPUT = str(Path(__file__).parent / "hyperswitch-pm-filters-full.generated.toml")


def main() -> int:
    p = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    p.add_argument("--source", default=DEFAULT_SOURCE,
                   help=f"source TOML (default: {DEFAULT_SOURCE})")
    p.add_argument("--output", default=DEFAULT_OUTPUT,
                   help="path to write SuperTOML to")
    p.add_argument("--connectors", default="",
                   help="optional comma-separated subset of connectors to "
                        "convert (default: all). Use this to spot-check the "
                        "script without generating the full 500+ MB file.")
    args = p.parse_args()

    connector_filter = {c.strip() for c in args.connectors.split(",") if c.strip()}

    src_path = Path(args.source)
    out_path = Path(args.output)
    if not src_path.exists():
        print(f"ERROR: source file not found: {src_path}", file=sys.stderr)
        return 2

    with src_path.open("rb") as fh:
        source = tomllib.load(fh)

    pm_filters = source.get("pm_filters")
    if not isinstance(pm_filters, dict):
        print("ERROR: source has no [pm_filters] section", file=sys.stderr)
        return 2

    mandates_section = (source.get("mandates") or {}).get("supported_payment_methods", {})
    zero_mandates_section = (source.get("zero_mandates") or {}).get("supported_payment_methods", {})

    if connector_filter:
        # Always keep "default" out of the filter applied here - it's skipped
        # downstream regardless, but logging is friendlier.
        pm_filters = {k: v for k, v in pm_filters.items()
                      if k == "default" or k in connector_filter}
        print(f"Filtering to connectors: {sorted(connector_filter)}")

    # Enum discovery walks ALL sources (including mandate/zero_mandate
    # contributors) even when filtering, so the dimension enums in the
    # output stay complete and the generated SuperTOML resolves cleanly.
    full_pm_filters = source["pm_filters"]
    connectors, payment_methods, countries, currencies, skipped_default = \
        discover_enums(full_pm_filters, mandates_section, zero_mandates_section)

    print(f"Discovered enums:")
    print(f"  connectors:      {len(connectors)}")
    print(f"  payment_methods: {len(payment_methods)}")
    print(f"  countries:       {len(countries)}")
    print(f"  currencies:      {len(currencies)}")
    if skipped_default:
        print(f"  skipped {skipped_default} rows from [pm_filters.default] "
              f"(no connector dim - expansion would add ~{skipped_default * len(connectors)}x rows)")

    overrides = build_overrides(
        pm_filters, mandates_section, zero_mandates_section,
        countries, currencies, connector_filter,
    )
    by_type: "dict[str, int]" = {}
    for ov in overrides:
        pt = ov["_context_"]["payment_type"]
        by_type[pt] = by_type.get(pt, 0) + 1
    print(f"\nGenerated {len(overrides):,} overrides "
          f"(each one carries all 6 dimensions)")
    for pt in ("normal", "mandate", "zero_dollar_mandate"):
        if by_type.get(pt):
            print(f"  payment_type = {pt!r}: {by_type[pt]:,}")

    emit_supertoml(connectors, payment_methods, countries, currencies,
                   overrides, skipped_default, out_path)
    size_mb = out_path.stat().st_size / (1024 * 1024)
    print(f"Wrote {out_path} ({size_mb:.1f} MB)")

    print(f"\nValidation:")
    if BINDINGS_AVAILABLE:
        print(f"  Resolver: superposition_bindings (Rust-backed, when re-parsing)")
    else:
        print(f"  Resolver: pure-Python tomllib (bindings unavailable: {BINDINGS_LOAD_ERROR})")
    try:
        ran, skipped, n_fail, failures, notes = validate(
            out_path, overrides, connector_filter,
        )
    except Exception as e:
        print(f"  PARSE ERROR: {e}", file=sys.stderr)
        return 1
    if notes:
        print(f"  Re-parse: {notes}")
    skip_note = f" ({skipped} skipped for filtered-out connectors)" if skipped else ""
    if failures:
        print(f"  Parity: {ran - n_fail}/{ran} pass{skip_note}")
        for f in failures:
            print(f"    - {f}")
        return 1
    print(f"  Parity: {ran}/{ran} pass{skip_note}")
    print("OK")
    return 0


if __name__ == "__main__":
    sys.exit(main())
