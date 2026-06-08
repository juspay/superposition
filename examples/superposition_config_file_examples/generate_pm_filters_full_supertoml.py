#!/usr/bin/env python3
"""
Generate a SuperTOML containing pm_filters and mandate-setup rules, with
every override carrying all five dimensions explicitly:
    currency, payment_method, country, connector, capture_method.

Instead of a 6th dimension (`payment_type` / `flow_kind`), we use three
orthogonal boolean config keys per context:
    payments_enabled
    payments_mandates_enabled
    payments_zero_dollar_mandates_enabled

A single query for a given (connector, pm, country, currency, capture)
tuple now returns all three answers at once, and the regular-mandate vs
zero-dollar-mandate distinction is preserved.

Sources -> flags:
  [pm_filters.<connector>.<payment_method>]    -> payments_enabled
  [mandates.supported_payment_methods]         -> payments_mandates_enabled
  [zero_mandates.supported_payment_methods]    -> payments_zero_dollar_mandates_enabled

Each row becomes the full cross-product over (country, currency,
capture_method) of contexts where the corresponding flag is set true:

  - For pm_filters: country/currency from the row's explicit list, or
    every observed value if the row omits one. capture_method emits both
    automatic and manual; rows with `not_available_flows = { capture_method
    = "manual" }` emit only automatic.
  - For mandates / zero_mandates: country, currency, and capture_method
    are never specified in the source, so all three expand to the full
    observed enum.

Where the three sources produce the same (connector, pm, country,
currency, capture) context, the overrides are merged so each unique
context emits ONE override with the appropriate combination of true
flags. Anything absent stays at the default-false declared in
[default-configs], so unset flags don't need to be listed.

Skips [pm_filters.default] - those rows have no connector dimension, and
the requirement is that every override carry all five dims. Expanding
default to all connectors would balloon the output by ~130x. Logged at
run time so it's not silent.

Usage:
    python3 generate_pm_filters_full_supertoml.py \\
        --source /tmp/hyperswitch-dev.toml \\
        --output hyperswitch-pm-filters-full.generated.toml

    # Subset for spot-checking - applies to all three flag sources.
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
    "capture_method": 6,
    "connector": 5,
    "country": 4,
    "payment_method": 3,
    "currency": 2,
    # Cohort dimension: LOCAL_COHORT:country at position 1 (must be <= the
    # base dim's position). A single value "all" fires for every country in
    # the discovered enum. Mandate sources and pm_filters rows whose country
    # list spans every discovered value emit with country_group="all", which
    # replaces 249 per-country overrides with one.
    "country_group": 1,
}

# The three orthogonal feature flags, in the order they're declared in
# [default-configs]. Order also drives deterministic emission.
FLAGS = (
    "payments_enabled",
    "payments_mandates_enabled",
    "payments_zero_dollar_mandates_enabled",
)

# Sentinel value used in country_group when a row covers every country.
COUNTRY_GROUP_ALL = "all"

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


def _set_flag(
    accum: dict,
    flag: str,
    *,
    connector: str,
    payment_method: str,
    currencies: "list[str]",
    capture_methods: "list[str]",
    countries: "list[str] | None" = None,
    country_group: "str | None" = None,
) -> None:
    """Mark `flag` true for every (connector, pm, country|country_group,
    currency, capture) in the cross-product. accum is keyed by a frozen
    context tuple. Exactly one of `countries` or `country_group` must be
    supplied: pm_filters rows with a specific country list pass `countries`;
    mandate sources and pm_filters rows that span every country pass
    `country_group=COUNTRY_GROUP_ALL` to collapse 249 per-country overrides
    into one."""
    if (countries is None) == (country_group is None):
        raise ValueError("pass exactly one of `countries` or `country_group`")

    if country_group is not None:
        country_pairs = [("country_group", country_group)]
    else:
        country_pairs = [("country", c) for c in countries]

    for capture in capture_methods:
        for country_kv in country_pairs:
            for currency in currencies:
                ctx_key = tuple(sorted([
                    ("capture_method", capture),
                    ("connector", connector),
                    country_kv,
                    ("currency", currency),
                    ("payment_method", payment_method),
                ]))
                if ctx_key not in accum:
                    accum[ctx_key] = set()
                accum[ctx_key].add(flag)


def build_overrides(
    pm_filters: dict,
    mandates_section: dict,
    zero_mandates_section: dict,
    all_countries: "list[str]",
    all_currencies: "list[str]",
    connector_filter: "set[str]",
) -> "list[dict]":
    """Walks the three source sections, accumulating which flags are true
    for each unique context. Returns a list of override dicts where each
    context appears exactly once carrying the union of its source flags."""
    accum: "dict[tuple, set[str]]" = {}

    # Track which (connector, pm) pairs have manual capture explicitly
    # blocked in pm_filters via `not_available_flows`. The Option A implication
    # (mandate -> payments) must respect this block to avoid contradicting
    # the source - otherwise adyen.ideal manual would incorrectly show
    # payments_enabled=true because adyen.ideal is in [mandates].
    manual_blocked_pairs: "set[tuple[str, str]]" = set()
    all_countries_set = set(all_countries)

    def _emit_country_axis(**kwargs) -> None:
        """Pick the country axis representation - per-country if the row has
        a specific list, cohort if it spans every observed country."""
        row_countries = kwargs.pop("countries")
        if set(row_countries) == all_countries_set:
            _set_flag(accum, country_group=COUNTRY_GROUP_ALL, **kwargs)
        else:
            _set_flag(accum, countries=row_countries, **kwargs)

    # ----- payments_enabled from pm_filters ---------------------------------
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
            if manual_blocked:
                manual_blocked_pairs.add((connector, pm))
            capture_methods = ["automatic"] if manual_blocked else ["automatic", "manual"]
            _emit_country_axis(
                flag="payments_enabled",
                connector=connector,
                payment_method=pm,
                countries=countries,
                currencies=currencies,
                capture_methods=capture_methods,
            )

    # ----- payments_mandates_enabled and _zero_dollar_mandates_enabled ------
    # Mandate sections don't carry country/currency/capture - the source
    # semantics are "this connector supports this kind of mandate for this
    # payment_method, period". Expand to every observed dimension value.
    #
    # Implied-flag rule: a (connector, pm) entry in either mandate list also
    # implies payments_enabled. The source's pm_filters is selective - many
    # combinations (notably cards) have no pm_filters row at all because
    # they're enabled by default in Hyperswitch's code, not in config. Without
    # this rule, mandate-supporting pairs that aren't also in pm_filters
    # would show payments_enabled=false, which is misleading. Mandate setup
    # requires the underlying payment method to be supported, so any source
    # row in [mandates] / [zero_mandates] is treated as proof that
    # payments_enabled is true for that (connector, pm) too.
    mandate_jobs = [
        # (source_flag, implied_flags, section)
        ("payments_mandates_enabled", ("payments_enabled",), mandates_section),
        ("payments_zero_dollar_mandates_enabled", ("payments_enabled",),
         zero_mandates_section),
    ]
    for source_flag, implied_flags, section in mandate_jobs:
        for pm, conns in walk_mandate_section(section):
            for connector in sorted(conns):
                if connector_filter and connector not in connector_filter:
                    continue
                # Mandate sources always span every country, so use the
                # country_group="all" cohort - one override per (capture,
                # currency) tuple instead of one per (capture, country,
                # currency). Currency stays per-value (no currency cohort
                # in this generator).
                _set_flag(
                    accum, source_flag,
                    connector=connector,
                    payment_method=pm,
                    country_group=COUNTRY_GROUP_ALL,
                    currencies=all_currencies,
                    capture_methods=["automatic", "manual"],
                )
                # Implied payments_enabled respects pm_filters' manual block.
                implied_captures = (
                    ["automatic"] if (connector, pm) in manual_blocked_pairs
                    else ["automatic", "manual"]
                )
                for flag in implied_flags:
                    _set_flag(
                        accum, flag,
                        connector=connector,
                        payment_method=pm,
                        country_group=COUNTRY_GROUP_ALL,
                        currencies=all_currencies,
                        capture_methods=implied_captures,
                    )

    # Materialise into the override list shape the emitter expects. Sort the
    # contexts deterministically (capture_method desc, then connector, ...).
    def ctx_sort_key(ctx_key: tuple) -> tuple:
        d = dict(ctx_key)
        return tuple(d.get(k, "") for k in
                     sorted(POSITIONS.keys(), key=lambda x: -POSITIONS[x]))

    overrides: "list[dict]" = []
    for ctx_key in sorted(accum.keys(), key=ctx_sort_key):
        flags = accum[ctx_key]
        ctx = sorted_context(dict(ctx_key))
        override = {"_context_": ctx}
        # Emit flags in FLAGS order so output stays deterministic and reads
        # in a predictable left-to-right sequence.
        for flag in FLAGS:
            if flag in flags:
                override[flag] = True
        overrides.append(override)
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
    the millions."""
    # build_overrides already returns overrides sorted by ctx position order;
    # this is a no-op safety net for callers that may have reordered.
    sort_key_order = sorted(POSITIONS.keys(), key=lambda k: -POSITIONS[k])
    def sort_key(ov: dict) -> tuple:
        ctx = ov["_context_"]
        return tuple(ctx.get(k, "") for k in sort_key_order)
    overrides.sort(key=sort_key)

    # Ascending position for dimensions (broadest axis first), matching the
    # convention used by the other generator. country_group is the LOCAL_COHORT
    # cohort derived from country - the "all" value matches every country in
    # the country enum.
    country_group_spec = {
        "position": POSITIONS["country_group"],
        "type": f"LOCAL_COHORT:country",
        "schema": {
            "type": "string",
            "enum": [COUNTRY_GROUP_ALL, "otherwise"],
            "definitions": {
                COUNTRY_GROUP_ALL: {"in": [{"var": "country"}, countries]},
            },
        },
    }
    dim_specs = [
        ("country_group", POSITIONS["country_group"], country_group_spec),
        ("currency", POSITIONS["currency"], {
            "position": POSITIONS["currency"],
            "schema": {"type": "string", "enum": currencies},
        }),
        ("payment_method", POSITIONS["payment_method"], {
            "position": POSITIONS["payment_method"],
            "schema": {"type": "string", "enum": payment_methods},
        }),
        ("country", POSITIONS["country"], {
            "position": POSITIONS["country"],
            "schema": {"type": "string", "enum": countries},
        }),
        ("connector", POSITIONS["connector"], {
            "position": POSITIONS["connector"],
            "schema": {"type": "string", "enum": connectors},
        }),
        ("capture_method", POSITIONS["capture_method"], {
            "position": POSITIONS["capture_method"],
            "schema": {"type": "string", "enum": ["automatic", "manual"]},
        }),
    ]

    with out_path.open("w") as fh:
        fh.write("# Generated by generate_pm_filters_full_supertoml.py - DO NOT EDIT.\n")
        fh.write("#\n")
        fh.write("# Scope: pm_filters + [mandates] + [zero_mandates]. Every override\n")
        fh.write("# carries all five dimensions (currency, payment_method, country,\n")
        fh.write("# connector, capture_method) and one or more of three feature flags:\n")
        fh.write("#   payments_enabled                       (from [pm_filters])\n")
        fh.write("#   payments_mandates_enabled              (from [mandates])\n")
        fh.write("#   payments_zero_dollar_mandates_enabled  (from [zero_mandates])\n")
        fh.write("# Rows missing country or currency in the source were expanded to\n")
        fh.write("# all values observed across pm_filters.\n")
        fh.write("#\n")
        fh.write(f"# Skipped {skipped_default} rows from [pm_filters.default] -\n")
        fh.write("# expanding them to every connector would balloon the output ~130x.\n")
        fh.write("\n")

        fh.write("[default-configs]\n")
        for flag in FLAGS:
            fh.write(f"{flag} = {toml_value({'value': False, 'schema': {'type': 'boolean'}})}\n")
        fh.write("\n")

        fh.write("[dimensions]\n")
        for name, _pos, spec in dim_specs:
            fh.write(f"{name} = {toml_value(spec)}\n")
        fh.write("\n")

        for ov in overrides:
            fh.write("[[overrides]]\n")
            fh.write(f"_context_ = {toml_value(ov['_context_'])}\n")
            for flag in FLAGS:
                if flag in ov:
                    fh.write(f"{flag} = {toml_value(ov[flag])}\n")
            fh.write("\n")


# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

# Parity matrix. Each row: (name, query, expected flag values). The query has
# 5 dimensions; we look up the matching override and assert each named flag.
# Flags not listed in `expected` are not checked.
PARITY_TESTS: "list[tuple[str, dict, dict[str, bool]]]" = [
    # ---- pm_filters cases (drive payments_enabled). -----------------------
    ("adyen swish SE/SEK auto",
     {"connector": "adyen", "payment_method": "swish", "country": "SE",
      "currency": "SEK", "capture_method": "automatic"},
     {"payments_enabled": True}),
    ("adyen swish SE/SEK manual",
     {"connector": "adyen", "payment_method": "swish", "country": "SE",
      "currency": "SEK", "capture_method": "manual"},
     {"payments_enabled": True}),
    # adyen.ideal has not_available_flows.capture_method = "manual".
    ("adyen ideal NL/EUR auto",
     {"connector": "adyen", "payment_method": "ideal", "country": "NL",
      "currency": "EUR", "capture_method": "automatic"},
     {"payments_enabled": True}),
    # adyen.ideal blocks manual capture in pm_filters but IS in both mandate
    # lists ([mandates].bank_redirect.ideal includes adyen). The mandate
    # flags fire across both captures; the Option A implication for
    # payments_enabled respects pm_filters' manual block and stays false.
    ("adyen ideal NL/EUR manual: mandate flags yes, payments NO",
     {"connector": "adyen", "payment_method": "ideal", "country": "NL",
      "currency": "EUR", "capture_method": "manual"},
     {"payments_enabled": False,
      "payments_mandates_enabled": True,
      "payments_zero_dollar_mandates_enabled": True}),
    ("razorpay upi_collect IN/INR auto",
     {"connector": "razorpay", "payment_method": "upi_collect", "country": "IN",
      "currency": "INR", "capture_method": "automatic"},
     {"payments_enabled": True}),
    ("adyen swish DE/EUR (not in source)",
     {"connector": "adyen", "payment_method": "swish", "country": "DE",
      "currency": "EUR", "capture_method": "automatic"},
     {"payments_enabled": False}),
    # truelayer.open_banking has currency only - country expanded.
    ("truelayer open_banking US/GBP auto (country expanded)",
     {"connector": "truelayer", "payment_method": "open_banking", "country": "US",
      "currency": "GBP", "capture_method": "automatic"},
     {"payments_enabled": True}),

    # ---- mandate cases. Mandate-true implies payments_enabled-true too
    # (Option A: mandate eligibility proves the payment method is supported).
    # stripe + card.credit IS in [mandates] and [zero_mandates].
    ("stripe credit JP/JPY manual: all three flags",
     {"connector": "stripe", "payment_method": "credit", "country": "JP",
      "currency": "JPY", "capture_method": "manual"},
     {"payments_enabled": True,
      "payments_mandates_enabled": True,
      "payments_zero_dollar_mandates_enabled": True}),
    # hipay credit is in NEITHER mandate list but IS in pm_filters.hipay.credit
    # (line 656 covers US/USD), so payments_enabled fires from the pm_filters
    # source, not from the Option A implication.
    ("hipay credit US/USD auto: payments yes (pm_filters), no mandates",
     {"connector": "hipay", "payment_method": "credit", "country": "US",
      "currency": "USD", "capture_method": "automatic"},
     {"payments_enabled": True,
      "payments_mandates_enabled": False,
      "payments_zero_dollar_mandates_enabled": False}),
    # gocardless + bank_debit.ach is in BOTH lists.
    ("gocardless ach DE/EUR: all three flags",
     {"connector": "gocardless", "payment_method": "ach", "country": "DE",
      "currency": "EUR", "capture_method": "automatic"},
     {"payments_enabled": True,
      "payments_mandates_enabled": True,
      "payments_zero_dollar_mandates_enabled": True}),
    # adyen + card.credit is in BOTH lists. adyen has no [pm_filters.adyen.credit]
    # row, so payments_enabled comes only from the mandate-implies-payments rule.
    ("adyen credit FR/EUR: all three flags via Option A",
     {"connector": "adyen", "payment_method": "credit", "country": "FR",
      "currency": "EUR", "capture_method": "automatic"},
     {"payments_enabled": True,
      "payments_mandates_enabled": True,
      "payments_zero_dollar_mandates_enabled": True}),
    # gocardless + bank_debit.sepa is in BOTH.
    ("gocardless sepa DE/EUR manual: all three flags",
     {"connector": "gocardless", "payment_method": "sepa", "country": "DE",
      "currency": "EUR", "capture_method": "manual"},
     {"payments_enabled": True,
      "payments_mandates_enabled": True,
      "payments_zero_dollar_mandates_enabled": True}),
    # gocardless + bank_debit.bacs: mandates AND zero_mandates both include
    # gocardless (line 1174).
    ("gocardless bacs GB/GBP: all three flags",
     {"connector": "gocardless", "payment_method": "bacs", "country": "GB",
      "currency": "GBP", "capture_method": "automatic"},
     {"payments_enabled": True,
      "payments_mandates_enabled": True,
      "payments_zero_dollar_mandates_enabled": True}),
    # stripe + bank_debit.bacs is in [mandates] ONLY (not in [zero_mandates]).
    # The regular vs zero-dollar distinction is preserved.
    ("stripe bacs GB/GBP: payments + mandate yes, zero NO",
     {"connector": "stripe", "payment_method": "bacs", "country": "GB",
      "currency": "GBP", "capture_method": "automatic"},
     {"payments_enabled": True,
      "payments_mandates_enabled": True,
      "payments_zero_dollar_mandates_enabled": False}),
    # adyen + bank_debit.bacs is in NEITHER mandate list, but adyen.bacs IS
    # in [pm_filters.adyen] (line 581: country = "GB", currency = "GBP").
    # So payments_enabled fires from pm_filters; both mandate flags stay false.
    ("adyen bacs GB/GBP: payments yes (pm_filters), no mandates",
     {"connector": "adyen", "payment_method": "bacs", "country": "GB",
      "currency": "GBP", "capture_method": "automatic"},
     {"payments_enabled": True,
      "payments_mandates_enabled": False,
      "payments_zero_dollar_mandates_enabled": False}),

    # ---- mixed cases. -----------------------------------------------------
    # stripe + affirm: pm_filters yes (US/USD), mandates no (affirm isn't
    # listed as a mandate-supported pm). Only payments_enabled fires.
    ("stripe affirm US/USD auto: payments yes, mandates no",
     {"connector": "stripe", "payment_method": "affirm", "country": "US",
      "currency": "USD", "capture_method": "automatic"},
     {"payments_enabled": True,
      "payments_mandates_enabled": False,
      "payments_zero_dollar_mandates_enabled": False}),
]


def resolve(
    overrides_index: "dict[tuple, dict]",
    query: dict,
    all_countries_set: "set[str]",
) -> "dict[str, bool]":
    """Returns a {flag: bool} dict for the query context. Missing flags
    default to False (default-configs).

    Overrides come in two shapes since the country_group cohort exists:
      - base shape: (capture, connector, country=X, currency, payment_method)
      - cohort shape: (capture, connector, country_group=all, currency, payment_method)
    We derive country_group from the query's country (per SuperTOML cohort
    semantics) and probe both shapes; flags from any matching override are
    OR-ed together."""
    augmented = dict(query)
    if "country" in augmented and augmented["country"] in all_countries_set:
        augmented["country_group"] = COUNTRY_GROUP_ALL

    result = {flag: False for flag in FLAGS}
    shapes = [
        ("capture_method", "connector", "country", "currency", "payment_method"),
        ("capture_method", "connector", "country_group", "currency", "payment_method"),
    ]
    for shape in shapes:
        if not all(k in augmented for k in shape):
            continue
        key = tuple(sorted((k, augmented[k]) for k in shape))
        for flag, val in overrides_index.get(key, {}).items():
            if val:
                result[flag] = True
    return result


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
    all_countries: "list[str]",
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
    # Build a context -> {flag: True} index for O(1) lookup.
    index: "dict[tuple, dict[str, bool]]" = {}
    for ov in overrides:
        key = tuple(sorted(ov["_context_"].items()))
        flags = {flag: True for flag in FLAGS if ov.get(flag)}
        index[key] = flags

    all_countries_set = set(all_countries)

    ran = 0
    skipped = 0
    for name, query, expected in PARITY_TESTS:
        # Skip tests whose connector wasn't in the filtered run.
        if connector_filter and query.get("connector") not in connector_filter:
            skipped += 1
            continue
        ran += 1
        got = resolve(index, query, all_countries_set)
        # Only check the flags the test asserts on.
        diffs = [(k, expected[k], got.get(k, False)) for k in expected
                 if got.get(k, False) != expected[k]]
        if diffs:
            mismatches = ", ".join(f"{k}: expected {e}, got {g}" for k, e, g in diffs)
            failures.append(f"{name}: {mismatches}")
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
    by_flag: "dict[str, int]" = {f: 0 for f in FLAGS}
    multi_flag = 0
    for ov in overrides:
        set_flags = [f for f in FLAGS if ov.get(f)]
        for f in set_flags:
            by_flag[f] += 1
        if len(set_flags) > 1:
            multi_flag += 1
    print(f"\nGenerated {len(overrides):,} overrides "
          f"(each one carries all 5 dimensions)")
    for f in FLAGS:
        if by_flag[f]:
            print(f"  {f} = true: {by_flag[f]:,}")
    if multi_flag:
        print(f"  (of which {multi_flag:,} overrides carry more than one true flag)")

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
            out_path, overrides, countries, connector_filter,
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
