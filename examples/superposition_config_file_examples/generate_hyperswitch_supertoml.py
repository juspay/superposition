#!/usr/bin/env python3
"""
Generate a focused Hyperswitch payment-method SuperTOML from development.toml.

This generator intentionally covers only the PM-filter-related migration plan:

  - dimensions: capture_method, connector, payment_method, country, currency
  - default configs:
      payments_enabled
      payments_mandates_enabled
      payments_zero_dollar_mandates_enabled
  - no country_group / region cohort in v1
  - runtime context still uses the five regular dimensions
  - generated overrides use only scalar context values for UI compatibility
  - large repeated value sets are represented as generated LOCAL_COHORT dimensions
  - small value sets are scalar-expanded instead of using list-valued contexts

The source of truth is Hyperswitch's config/development.toml. The generated
file is deterministic: the same input file produces byte-for-byte identical
output. Validation also checks the local Hyperswitch Rust code contract that
this translation depends on.

Usage:
    python3 examples/superposition_config_file_examples/generate_hyperswitch_supertoml.py

    python3 examples/superposition_config_file_examples/generate_hyperswitch_supertoml.py \\
        --source /path/to/hyperswitch/config/development.toml \\
        --output /tmp/hyperswitch-payment-methods.generated.toml

    python3 examples/superposition_config_file_examples/generate_hyperswitch_supertoml.py \\
        --validate-only --output ./hyperswitch-development.generated.toml
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import subprocess
import sys
import tomllib
from collections import OrderedDict, defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable


UNSPECIFIED = "__unspecified__"

DIMENSION_ORDER = [
    "capture_method",
    "connector",
    "payment_method",
    "country",
    "currency",
]

REGULAR_DIMENSION_ORDER = [
    "currency",
    "country",
    "payment_method",
    "connector",
    "capture_method",
]

CONTEXT_DIMENSION_ORDER = [
    "capture_method",
    "connector",
    "payment_method",
    "country",
    "currency",
]

COHORTABLE_DIMENSIONS = {"connector", "country", "currency"}
COHORT_MATCH_VALUE = "in"
COHORT_FALLBACK_VALUE = "otherwise"

# Keep scalar expansion deliberately small when choosing cohort candidates.
# Only the highest-impact candidates become generated local cohorts; the rest
# are scalar-expanded so the final artifact stays within Superposition's
# position/weight limits.
SCALAR_CONTEXT_EXPANSION_LIMIT = 2
MAX_COHORT_DIMENSIONS = 25

# Values from Hyperswitch's common_enums::CaptureMethod.
CAPTURE_METHODS = [
    UNSPECIFIED,
    "automatic",
    "manual",
    "manual_multiple",
    "scheduled",
    "sequential_automatic",
]

DEFAULT_OUTPUT = (
    Path(__file__).resolve().parent / "hyperswitch-development.generated.toml"
)


@dataclass(frozen=True)
class RawPmFilterRow:
    connector: str
    payment_method: str
    countries: tuple[str, ...] | None
    currencies: tuple[str, ...] | None
    blocked_capture_methods: tuple[str, ...]
    source: str


@dataclass(frozen=True)
class GroupedPmFilterRow:
    connectors: tuple[str, ...]
    payment_method: str
    countries: tuple[str, ...] | None
    currencies: tuple[str, ...] | None
    blocked_capture_methods: tuple[str, ...]
    source: str


@dataclass(frozen=True)
class GeneratedCohort:
    name: str
    based_on: str
    values: tuple[str, ...]


@dataclass(frozen=True)
class OverrideIntent:
    connectors: tuple[str, ...]
    payment_method: str
    countries: tuple[str, ...]
    currencies: tuple[str, ...]
    capture_methods: tuple[str, ...]
    configs: tuple[tuple[str, Any], ...]


@dataclass
class SourceModel:
    source_path: Path
    source_sha256: str
    hyperswitch_commit: str | None
    connector_universe: tuple[str, ...]
    payment_methods: tuple[str, ...]
    countries: tuple[str, ...]
    currencies: tuple[str, ...]
    pm_rows_by_connector: dict[str, dict[str, RawPmFilterRow]]
    default_pm_rows: dict[str, RawPmFilterRow]
    grouped_pm_rows: tuple[GroupedPmFilterRow, ...]
    mandate_connectors_by_pm: dict[str, tuple[str, ...]]
    zero_mandate_connectors_by_pm: dict[str, tuple[str, ...]]


def split_csv(value: Any) -> tuple[str, ...]:
    if not isinstance(value, str):
        return tuple()
    return tuple(sorted({item.strip() for item in value.split(",") if item.strip()}))


def sorted_with_unspecified(values: Iterable[str]) -> tuple[str, ...]:
    return tuple(sorted(set(values), key=lambda item: (item != UNSPECIFIED, item)))


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as file:
        for chunk in iter(lambda: file.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def discover_source() -> Path | None:
    candidates: list[Path] = []
    env_source = os.environ.get("HYPERSWITCH_DEV_TOML")
    if env_source:
        candidates.append(Path(env_source))

    candidates.extend(
        [
            Path.home() / "hyperswitch/hyperswitch/config/development.toml",
            Path("/tmp/hyperswitch-dev.toml"),
            Path("/private/tmp/hyperswitch-dev.toml"),
        ]
    )

    for candidate in candidates:
        if candidate.is_file():
            return candidate
    return None


def hyperswitch_root_from_source(source_path: Path) -> Path:
    # .../hyperswitch/config/development.toml -> .../hyperswitch
    return source_path.resolve().parents[1]


def git_commit_for(path: Path) -> str | None:
    try:
        proc = subprocess.run(
            ["git", "-C", str(path), "rev-parse", "HEAD"],
            check=True,
            capture_output=True,
            text=True,
        )
    except Exception:
        return None
    return proc.stdout.strip() or None


def verify_hyperswitch_code_contract(root: Path) -> list[str]:
    checks = [
        (
            root / "crates/router/src/configs/settings.rs",
            [
                "pub struct ConnectorFilters(pub HashMap<String, PaymentMethodFilters>);",
                "pub struct PaymentMethodFilters(pub HashMap<PaymentMethodFilterKey, CurrencyCountryFlowFilter>);",
                "pub currency: Option<HashSet<enums::Currency>>",
                "pub country: Option<HashSet<enums::CountryAlpha2>>",
                "pub not_available_flows: Option<NotAvailableFlows>",
                "pub capture_method: Option<enums::CaptureMethod>",
            ],
        ),
        (
            root / "crates/router/src/core/payments/payment_methods.rs",
            [
                ".pm_filters",
                ".get(connector)",
                '.or_else(|| config.pm_filters.0.get("default"))',
                "filter_config_country_currency_based(value, country, currency)",
                ".unwrap_or(true)",
                "country_condition.unwrap_or(true) && currency_condition.unwrap_or(true)",
            ],
        ),
        (
            root / "crates/router/src/core/payment_methods/utils.rs",
            [
                "construct_capture_method_node",
                ".and_then(|v| v.not_available_flows)",
                ".and_then(|v| v.capture_method)",
                "common_enums::CaptureMethod::Manual",
            ],
        ),
        (
            root / "crates/common_enums/src/enums.rs",
            [
                "pub enum CaptureMethod",
                "Automatic",
                "Manual",
                "ManualMultiple",
                "Scheduled",
                "SequentialAutomatic",
            ],
        ),
    ]

    errors: list[str] = []
    for path, needles in checks:
        if not path.is_file():
            errors.append(f"missing expected Hyperswitch source file: {path}")
            continue
        text = path.read_text()
        for needle in needles:
            if needle not in text:
                errors.append(f"{path}: missing expected code marker {needle!r}")
    return errors


def iter_connector_lists(tree: Any) -> Iterable[tuple[str, tuple[str, ...]]]:
    def walk(node: Any, path: list[str]) -> Iterable[tuple[str, tuple[str, ...]]]:
        if not isinstance(node, dict):
            return

        connector_list = node.get("connector_list")
        if isinstance(connector_list, str) and path:
            yield path[-1], split_csv(connector_list)

        for key, value in node.items():
            if key == "connector_list":
                continue
            yield from walk(value, [*path, key])

    yield from walk(tree, [])


def collect_connector_universe(source: dict[str, Any]) -> tuple[str, ...]:
    connectors: set[str] = set()

    supported = source.get("connectors", {}).get("supported", {})
    if isinstance(supported, dict):
        for connector_list in supported.values():
            if isinstance(connector_list, list):
                connectors.update(
                    item for item in connector_list if isinstance(item, str)
                )

    connectors.update(
        key
        for key in source.get("connectors", {}).keys()
        if key != "supported" and isinstance(key, str)
    )
    connectors.update(
        key
        for key in source.get("pm_filters", {}).keys()
        if key != "default" and isinstance(key, str)
    )

    for pm_tree_name in ("mandates", "zero_mandates"):
        tree = (
            source.get(pm_tree_name, {})
            .get("supported_payment_methods", {})
        )
        for _, connector_list in iter_connector_lists(tree):
            connectors.update(connector_list)

    return tuple(sorted(connectors))


def parse_pm_row(connector: str, payment_method: str, spec: Any, source: str) -> RawPmFilterRow:
    if not isinstance(spec, dict):
        countries = None
        currencies = None
        blocked_capture_methods: tuple[str, ...] = tuple()
    else:
        country_values = split_csv(spec.get("country"))
        currency_values = split_csv(spec.get("currency"))
        countries = country_values or None
        currencies = currency_values or None

        blocked_capture_methods = tuple()
        not_available_flows = spec.get("not_available_flows")
        if isinstance(not_available_flows, dict):
            capture_method = not_available_flows.get("capture_method")
            if isinstance(capture_method, str):
                blocked_capture_methods = (capture_method.strip().lower(),)

    return RawPmFilterRow(
        connector=connector,
        payment_method=payment_method,
        countries=countries,
        currencies=currencies,
        blocked_capture_methods=blocked_capture_methods,
        source=source,
    )


def collect_raw_pm_rows(
    source: dict[str, Any],
    connector_universe: tuple[str, ...],
) -> tuple[
    dict[str, dict[str, RawPmFilterRow]],
    dict[str, RawPmFilterRow],
    tuple[RawPmFilterRow, ...],
]:
    pm_filters = source.get("pm_filters", {})
    if not isinstance(pm_filters, dict):
        return {}, {}, tuple()

    rows_by_connector: dict[str, dict[str, RawPmFilterRow]] = {}
    for connector in sorted(key for key in pm_filters.keys() if key != "default"):
        rows = pm_filters.get(connector)
        if not isinstance(rows, dict):
            continue
        rows_by_connector[connector] = {}
        for payment_method in sorted(rows.keys()):
            row = parse_pm_row(
                connector,
                payment_method,
                rows[payment_method],
                f"pm_filters.{connector}.{payment_method}",
            )
            rows_by_connector[connector][payment_method] = row

    default_pm_rows: dict[str, RawPmFilterRow] = {}
    default_rows = pm_filters.get("default", {})
    if isinstance(default_rows, dict):
        for payment_method in sorted(default_rows.keys()):
            default_pm_rows[payment_method] = parse_pm_row(
                "default",
                payment_method,
                default_rows[payment_method],
                f"pm_filters.default.{payment_method}",
            )

    connectors_with_specific_table = set(rows_by_connector.keys())
    connectors_without_specific_table = tuple(
        connector
        for connector in connector_universe
        if connector not in connectors_with_specific_table
    )

    effective_rows: list[RawPmFilterRow] = []
    for connector_rows in rows_by_connector.values():
        effective_rows.extend(connector_rows.values())

    for connector in connectors_without_specific_table:
        for default_row in default_pm_rows.values():
            effective_rows.append(
                RawPmFilterRow(
                    connector=connector,
                    payment_method=default_row.payment_method,
                    countries=default_row.countries,
                    currencies=default_row.currencies,
                    blocked_capture_methods=default_row.blocked_capture_methods,
                    source=f"{default_row.source} fallback for {connector}",
                )
            )

    return rows_by_connector, default_pm_rows, tuple(effective_rows)


def group_pm_rows(rows: Iterable[RawPmFilterRow]) -> tuple[GroupedPmFilterRow, ...]:
    grouped: dict[
        tuple[str, tuple[str, ...] | None, tuple[str, ...] | None, tuple[str, ...]],
        set[str],
    ] = defaultdict(set)
    source_labels: dict[
        tuple[str, tuple[str, ...] | None, tuple[str, ...] | None, tuple[str, ...]],
        set[str],
    ] = defaultdict(set)

    for row in rows:
        key = (
            row.payment_method,
            row.countries,
            row.currencies,
            row.blocked_capture_methods,
        )
        grouped[key].add(row.connector)
        source_labels[key].add(row.source.rsplit(" fallback for ", 1)[0])

    result = []
    for (
        payment_method,
        countries,
        currencies,
        blocked_capture_methods,
    ), connectors in grouped.items():
        result.append(
            GroupedPmFilterRow(
                connectors=tuple(sorted(connectors)),
                payment_method=payment_method,
                countries=countries,
                currencies=currencies,
                blocked_capture_methods=blocked_capture_methods,
                source=", ".join(sorted(source_labels[
                    (payment_method, countries, currencies, blocked_capture_methods)
                ])),
            )
        )

    return tuple(
        sorted(
            result,
            key=lambda row: (
                row.payment_method,
                row.countries or tuple(),
                row.currencies or tuple(),
                row.blocked_capture_methods,
                row.connectors,
            ),
        )
    )


def collect_mandate_connectors_by_pm(
    source: dict[str, Any],
    section_name: str,
) -> dict[str, tuple[str, ...]]:
    tree = source.get(section_name, {}).get("supported_payment_methods", {})
    connectors_by_pm: dict[str, set[str]] = defaultdict(set)
    for payment_method, connectors in iter_connector_lists(tree):
        connectors_by_pm[payment_method].update(connectors)
    return {
        payment_method: tuple(sorted(connectors))
        for payment_method, connectors in sorted(connectors_by_pm.items())
    }


def build_source_model(source_path: Path) -> SourceModel:
    source_text = source_path.read_text()
    source = tomllib.loads(source_text)
    connector_universe = collect_connector_universe(source)
    rows_by_connector, default_pm_rows, effective_rows = collect_raw_pm_rows(
        source,
        connector_universe,
    )
    grouped_rows = group_pm_rows(effective_rows)

    mandate_connectors_by_pm = collect_mandate_connectors_by_pm(source, "mandates")
    zero_mandate_connectors_by_pm = collect_mandate_connectors_by_pm(
        source,
        "zero_mandates",
    )

    payment_methods = set()
    countries = {UNSPECIFIED}
    currencies = {UNSPECIFIED}

    for row in effective_rows:
        payment_methods.add(row.payment_method)
        if row.countries:
            countries.update(row.countries)
        if row.currencies:
            currencies.update(row.currencies)

    for connectors_by_pm in (mandate_connectors_by_pm, zero_mandate_connectors_by_pm):
        payment_methods.update(connectors_by_pm.keys())

    root = hyperswitch_root_from_source(source_path)

    return SourceModel(
        source_path=source_path,
        source_sha256=sha256_file(source_path),
        hyperswitch_commit=git_commit_for(root),
        connector_universe=connector_universe,
        payment_methods=tuple(sorted(payment_methods)),
        countries=sorted_with_unspecified(countries),
        currencies=sorted_with_unspecified(currencies),
        pm_rows_by_connector=rows_by_connector,
        default_pm_rows=default_pm_rows,
        grouped_pm_rows=grouped_rows,
        mandate_connectors_by_pm=mandate_connectors_by_pm,
        zero_mandate_connectors_by_pm=zero_mandate_connectors_by_pm,
    )


def toml_value(value: Any) -> str:
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, str):
        return json.dumps(value)
    if isinstance(value, int):
        return str(value)
    if isinstance(value, list) or isinstance(value, tuple):
        return "[" + ", ".join(toml_value(item) for item in value) + "]"
    if isinstance(value, dict) or isinstance(value, OrderedDict):
        return "{ " + ", ".join(
            f"{key} = {toml_value(item)}" for key, item in value.items()
        ) + " }"
    raise TypeError(f"cannot encode {type(value).__name__} as TOML")


def context_value(values: Iterable[str]) -> str | list[str]:
    unique = tuple(sorted_with_unspecified(values))
    if not unique:
        raise ValueError("context list cannot be empty")
    if len(unique) == 1:
        return unique[0]
    return list(unique)


NAMED_COHORTS: dict[tuple[str, tuple[str, ...]], str] = {
    (
        "country",
        tuple(
            sorted(
                [
                    "AT",
                    "BE",
                    "BG",
                    "CY",
                    "CZ",
                    "DE",
                    "DK",
                    "EE",
                    "ES",
                    "FI",
                    "FR",
                    "GR",
                    "HR",
                    "HU",
                    "IE",
                    "IS",
                    "IT",
                    "LI",
                    "LT",
                    "LU",
                    "LV",
                    "MT",
                    "NL",
                    "NO",
                    "PL",
                    "PT",
                    "RO",
                    "SE",
                    "SI",
                    "SK",
                ]
            )
        ),
    ): "country_is_eea",
    ("country", ("CA", "US")): "country_is_us_ca",
    ("country", ("DK", "FI", "NO", "SE")): "country_is_nordics",
    ("currency", ("CAD", "USD")): "currency_is_usd_cad",
    ("currency", ("DKK", "EUR", "NOK", "SEK")): "currency_is_nordic_plus_eur",
    ("currency", ("EUR", "GBP")): "currency_is_eur_gbp",
}


def semantic_not_name(name: str) -> str:
    return name.replace("_is_", "_is_not_", 1)


def semantic_cohort_name_for(
    dimension: str,
    values: Iterable[str],
    universe: Iterable[str],
) -> str | None:
    value_tuple = tuple(sorted_with_unspecified(values))
    if named_cohort := NAMED_COHORTS.get((dimension, value_tuple)):
        return named_cohort

    value_set = set(value_tuple)
    real_universe = set(universe) - {UNSPECIFIED}
    for (named_dimension, named_values), named_cohort in NAMED_COHORTS.items():
        if named_dimension != dimension:
            continue
        if value_set == real_universe - set(named_values):
            return semantic_not_name(named_cohort)
    return None


def cohort_name_for(
    dimension: str,
    values: Iterable[str],
    universe: Iterable[str],
) -> str:
    digest_payload = json.dumps(
        {"dimension": dimension, "values": tuple(sorted_with_unspecified(values))},
        separators=(",", ":"),
    ).encode("utf-8")
    digest = hashlib.sha256(digest_payload).hexdigest()[:12]
    return f"generated_{dimension}_set_{digest}"


class ContextEncoder:
    def __init__(
        self,
        model: SourceModel,
        selected_cohort_keys: set[tuple[str, tuple[str, ...]]],
    ):
        self.model = model
        self.selected_cohort_keys = selected_cohort_keys
        self._cohorts_by_key: dict[tuple[str, tuple[str, ...]], GeneratedCohort] = {}

    def cohorts(self) -> tuple[GeneratedCohort, ...]:
        return tuple(
            sorted(
                self._cohorts_by_key.values(),
                key=lambda cohort: (cohort.based_on, cohort.name),
            )
        )

    def universe_for(self, dimension: str) -> tuple[str, ...]:
        match dimension:
            case "capture_method":
                return tuple(CAPTURE_METHODS)
            case "connector":
                return self.model.connector_universe
            case "payment_method":
                return self.model.payment_methods
            case "country":
                return self.model.countries
            case "currency":
                return self.model.currencies
        raise ValueError(f"unknown dimension {dimension!r}")

    def cohort_for(
        self,
        dimension: str,
        values: Iterable[str],
    ) -> GeneratedCohort:
        value_tuple = tuple(sorted_with_unspecified(values))
        key = (dimension, value_tuple)
        cohort = self._cohorts_by_key.get(key)
        if cohort is not None:
            return cohort

        universe = self.universe_for(dimension)
        cohort = GeneratedCohort(
            name=semantic_cohort_name_for(dimension, value_tuple, universe)
            or cohort_name_for(dimension, value_tuple, universe),
            based_on=dimension,
            values=value_tuple,
        )
        self._cohorts_by_key[key] = cohort
        return cohort

    def alternatives_for(
        self,
        dimension: str,
        values: Iterable[str],
    ) -> tuple[OrderedDict[str, Any], ...]:
        value_tuple = tuple(sorted_with_unspecified(values))
        if not value_tuple:
            raise ValueError(f"context values for {dimension} cannot be empty")

        universe = set(self.universe_for(dimension))
        value_set = set(value_tuple)
        unknown_values = sorted(value_set - universe)
        if unknown_values:
            raise ValueError(
                f"context values for {dimension} are not in the dimension universe: {unknown_values[:5]}"
            )

        if value_set == universe:
            # Omitted dimensions are Superposition's native wildcard.
            return (OrderedDict(),)

        key = (dimension, value_tuple)
        if dimension in COHORTABLE_DIMENSIONS and key in self.selected_cohort_keys:
            cohort = self.cohort_for(dimension, value_tuple)
            return (
                OrderedDict([(cohort.name, COHORT_MATCH_VALUE)]),
            )

        return tuple(
            OrderedDict([(dimension, value)])
            for value in value_tuple
        )

    def make_contexts(
        self,
        *,
        connectors: Iterable[str],
        payment_method: str,
        countries: Iterable[str],
        currencies: Iterable[str],
        capture_methods: Iterable[str],
    ) -> tuple[OrderedDict[str, Any], ...]:
        alternative_groups = [
            self.alternatives_for("capture_method", capture_methods),
            self.alternatives_for("connector", connectors),
            self.alternatives_for("payment_method", (payment_method,)),
            self.alternatives_for("country", countries),
            self.alternatives_for("currency", currencies),
        ]

        contexts = [OrderedDict()]
        for alternatives in alternative_groups:
            expanded_contexts: list[OrderedDict[str, Any]] = []
            for context in contexts:
                for alternative in alternatives:
                    next_context = OrderedDict(context)
                    next_context.update(alternative)
                    expanded_contexts.append(next_context)
            contexts = expanded_contexts

        if not contexts:
            raise ValueError("at least one context must be generated")
        return tuple(contexts)


def add_override(
    overrides_by_context: dict[str, dict[str, Any]],
    context: OrderedDict[str, Any],
    **values: Any,
) -> None:
    context_key = json.dumps(context, sort_keys=True)
    existing = overrides_by_context.setdefault(context_key, {"_context_": context})
    for key, value in values.items():
        previous = existing.get(key)
        if previous is not None and previous != value:
            raise ValueError(
                f"conflicting values for {key} in context {context}: {previous!r} vs {value!r}"
            )
        existing[key] = value


def build_override_intents(model: SourceModel) -> tuple[OverrideIntent, ...]:
    intents: list[OverrideIntent] = []
    all_countries = set(model.countries)
    all_currencies = set(model.currencies)
    real_countries = all_countries - {UNSPECIFIED}
    real_currencies = all_currencies - {UNSPECIFIED}

    def add_intent(
        *,
        connectors: Iterable[str],
        payment_method: str,
        countries: Iterable[str],
        currencies: Iterable[str],
        capture_methods: Iterable[str],
        values: dict[str, Any],
    ) -> None:
        intents.append(
            OverrideIntent(
                connectors=tuple(sorted_with_unspecified(connectors)),
                payment_method=payment_method,
                countries=tuple(sorted_with_unspecified(countries)),
                currencies=tuple(sorted_with_unspecified(currencies)),
                capture_methods=tuple(sorted_with_unspecified(capture_methods)),
                configs=tuple(sorted(values.items())),
            )
        )

    for row in model.grouped_pm_rows:
        if row.countries is not None:
            denied_countries = real_countries - set(row.countries)
            if denied_countries:
                add_intent(
                    connectors=row.connectors,
                    payment_method=row.payment_method,
                    countries=denied_countries,
                    currencies=all_currencies,
                    capture_methods=CAPTURE_METHODS,
                    values={"payments_enabled": False},
                )

        if row.currencies is not None:
            denied_currencies = real_currencies - set(row.currencies)
            if denied_currencies:
                add_intent(
                    connectors=row.connectors,
                    payment_method=row.payment_method,
                    countries=all_countries,
                    currencies=denied_currencies,
                    capture_methods=CAPTURE_METHODS,
                    values={"payments_enabled": False},
                )

        if row.blocked_capture_methods:
            add_intent(
                connectors=row.connectors,
                payment_method=row.payment_method,
                countries=all_countries,
                currencies=all_currencies,
                capture_methods=row.blocked_capture_methods,
                values={"payments_enabled": False},
            )

    for payment_method, connectors in model.mandate_connectors_by_pm.items():
        add_intent(
            connectors=connectors,
            payment_method=payment_method,
            countries=all_countries,
            currencies=all_currencies,
            capture_methods=CAPTURE_METHODS,
            values={"payments_mandates_enabled": True},
        )

    for payment_method, connectors in model.zero_mandate_connectors_by_pm.items():
        add_intent(
            connectors=connectors,
            payment_method=payment_method,
            countries=all_countries,
            currencies=all_currencies,
            capture_methods=CAPTURE_METHODS,
            values={"payments_zero_dollar_mandates_enabled": True},
        )

    return tuple(intents)


def intent_dimension_values(intent: OverrideIntent) -> dict[str, tuple[str, ...]]:
    return {
        "capture_method": intent.capture_methods,
        "connector": intent.connectors,
        "payment_method": (intent.payment_method,),
        "country": intent.countries,
        "currency": intent.currencies,
    }


def universe_for_model(model: SourceModel, dimension: str) -> tuple[str, ...]:
    match dimension:
        case "capture_method":
            return tuple(CAPTURE_METHODS)
        case "connector":
            return model.connector_universe
        case "payment_method":
            return model.payment_methods
        case "country":
            return model.countries
        case "currency":
            return model.currencies
    raise ValueError(f"unknown dimension {dimension!r}")


def scalar_context_size(
    model: SourceModel,
    dimension: str,
    values: tuple[str, ...],
) -> int:
    if set(values) == set(universe_for_model(model, dimension)):
        return 1
    return len(values)


def cohort_candidate_key(
    model: SourceModel,
    dimension: str,
    values: tuple[str, ...],
) -> tuple[str, tuple[str, ...]] | None:
    if dimension not in COHORTABLE_DIMENSIONS:
        return None
    if set(values) == set(universe_for_model(model, dimension)):
        return None

    universe = universe_for_model(model, dimension)
    if (
        semantic_cohort_name_for(dimension, values, universe) is not None
        or len(values) > SCALAR_CONTEXT_EXPANSION_LIMIT
    ):
        return (dimension, values)
    return None


def select_cohort_keys(
    model: SourceModel,
    intents: Iterable[OverrideIntent],
) -> set[tuple[str, tuple[str, ...]]]:
    candidate_scores: dict[tuple[str, tuple[str, ...]], int] = defaultdict(int)
    semantic_candidates: set[tuple[str, tuple[str, ...]]] = set()

    for intent in intents:
        dimension_values = intent_dimension_values(intent)
        scalar_sizes = {
            dimension: scalar_context_size(model, dimension, values)
            for dimension, values in dimension_values.items()
        }
        scalar_product = 1
        for size in scalar_sizes.values():
            scalar_product *= size

        for dimension, values in dimension_values.items():
            candidate = cohort_candidate_key(model, dimension, values)
            if candidate is None:
                continue

            universe = universe_for_model(model, dimension)
            if semantic_cohort_name_for(dimension, values, universe) is not None:
                semantic_candidates.add(candidate)

            size = scalar_sizes[dimension]
            if size <= 1:
                continue
            candidate_scores[candidate] += scalar_product - (scalar_product // size)

    selected = set(sorted(semantic_candidates))
    remaining_slots = max(0, MAX_COHORT_DIMENSIONS - len(selected))
    generated_candidates = [
        (candidate, score)
        for candidate, score in candidate_scores.items()
        if candidate not in selected
    ]
    generated_candidates.sort(
        key=lambda item: (
            -item[1],
            item[0][0],
            item[0][1],
        )
    )
    selected.update(candidate for candidate, _ in generated_candidates[:remaining_slots])
    return selected


def build_overrides(
    model: SourceModel,
) -> tuple[tuple[dict[str, Any], ...], tuple[GeneratedCohort, ...]]:
    overrides_by_context: dict[str, dict[str, Any]] = {}
    intents = build_override_intents(model)
    context_encoder = ContextEncoder(model, select_cohort_keys(model, intents))

    for intent in intents:
        for context in context_encoder.make_contexts(
            connectors=intent.connectors,
            payment_method=intent.payment_method,
            countries=intent.countries,
            currencies=intent.currencies,
            capture_methods=intent.capture_methods,
        ):
            add_override(overrides_by_context, context, **dict(intent.configs))

    return (
        tuple(sorted(overrides_by_context.values(), key=override_sort_key)),
        context_encoder.cohorts(),
    )


def scalar_or_list_sort_value(value: Any) -> tuple[str, ...]:
    if isinstance(value, list):
        return tuple(str(item) for item in value)
    return (str(value),)


def override_sort_key(override: dict[str, Any]) -> tuple[Any, ...]:
    context = override["_context_"]
    return (
        scalar_or_list_sort_value(context.get("payment_method", "")),
        tuple(
            (key, scalar_or_list_sort_value(value))
            for key, value in sorted(context.items())
        ),
        tuple(sorted(key for key in override.keys() if key != "_context_")),
    )


def emit_supertoml(model: SourceModel) -> str:
    overrides, generated_cohorts = build_overrides(model)
    lines: list[str] = []

    lines.extend(
        [
            "# Generated from Hyperswitch config/development.toml.",
            "# DO NOT EDIT BY HAND. Re-run generate_hyperswitch_supertoml.py.",
            "#",
            f"# Source path: {model.source_path}",
            f"# Source sha256: {model.source_sha256}",
        ]
    )
    if model.hyperswitch_commit:
        lines.append(f"# Hyperswitch commit: {model.hyperswitch_commit}")
    lines.extend(
        [
            "#",
            "# Scope: PM filters, mandate support, and zero-dollar mandate support.",
            "# Shape: override contexts use scalar values only; omitted dimensions are wildcards.",
            "# Generated LOCAL_COHORT dimensions are compiler-private compression helpers.",
            "",
            "[default-configs]",
            'payments_enabled = { value = true, schema = { type = "boolean" } }',
            'payments_mandates_enabled = { value = false, schema = { type = "boolean" } }',
            'payments_zero_dollar_mandates_enabled = { value = false, schema = { type = "boolean" } }',
            "",
            "[dimensions]",
        ]
    )

    dimension_specs: OrderedDict[str, dict[str, Any]] = OrderedDict()

    for position, cohort in enumerate(generated_cohorts, start=1):
        dimension_specs[cohort.name] = OrderedDict(
            [
                ("position", position),
                ("type", f"LOCAL_COHORT:{cohort.based_on}"),
                (
                    "schema",
                    OrderedDict(
                        [
                            ("type", "string"),
                            (
                                "enum",
                                [COHORT_MATCH_VALUE, COHORT_FALLBACK_VALUE],
                            ),
                            (
                                "definitions",
                                OrderedDict(
                                    [
                                        (
                                            COHORT_MATCH_VALUE,
                                            OrderedDict(
                                                [
                                                    (
                                                        "in",
                                                        [
                                                            OrderedDict(
                                                                [("var", cohort.based_on)]
                                                            ),
                                                            list(cohort.values),
                                                        ],
                                                    )
                                                ]
                                            ),
                                        )
                                    ]
                                ),
                            ),
                        ]
                    ),
                ),
            ]
        )

    regular_position_offset = len(generated_cohorts)
    regular_dimension_specs = {
        "currency": {"type": "string", "enum": list(model.currencies)},
        "country": {"type": "string", "enum": list(model.countries)},
        "payment_method": {"type": "string", "enum": list(model.payment_methods)},
        "connector": {"type": "string", "enum": list(model.connector_universe)},
        "capture_method": {"type": "string", "enum": CAPTURE_METHODS},
    }
    for relative_position, dimension in enumerate(REGULAR_DIMENSION_ORDER, start=1):
        dimension_specs[dimension] = OrderedDict(
            [
                ("position", regular_position_offset + relative_position),
                ("schema", regular_dimension_specs[dimension]),
            ]
        )

    for dimension, spec in dimension_specs.items():
        lines.append(f"{dimension} = {toml_value(spec)}")

    lines.append("")
    for override in overrides:
        lines.append("[[overrides]]")
        lines.append(f"_context_ = {toml_value(override['_context_'])}")
        for key in sorted(key for key in override.keys() if key != "_context_"):
            lines.append(f"{key} = {toml_value(override[key])}")
        lines.append("")

    return "\n".join(lines).rstrip() + "\n"


def value_matches(condition_value: Any, query_value: str) -> bool:
    if isinstance(condition_value, list):
        return query_value in condition_value
    return condition_value == query_value


def evaluate_jsonlogic(rule: Any, context: dict[str, str]) -> bool:
    if isinstance(rule, dict) and set(rule) == {"in"}:
        operands = rule["in"]
        if not isinstance(operands, list) or len(operands) != 2:
            return False
        needle, haystack = operands
        if isinstance(needle, dict) and set(needle) == {"var"}:
            needle = context.get(str(needle["var"]))
        if not isinstance(haystack, list):
            return False
        return needle in haystack
    if isinstance(rule, bool):
        return rule
    return False


def query_with_local_cohorts(
    parsed: dict[str, Any],
    query: dict[str, str],
) -> dict[str, str]:
    expanded_query = dict(query)
    dimensions = parsed.get("dimensions", {})

    for dimension, spec in sorted(
        dimensions.items(),
        key=lambda item: item[1].get("position", 0),
    ):
        dimension_type = spec.get("type")
        if not (
            isinstance(dimension_type, str)
            and dimension_type.startswith("LOCAL_COHORT:")
        ):
            continue

        matched_value = COHORT_FALLBACK_VALUE
        definitions = spec.get("schema", {}).get("definitions", {})
        for cohort_value, rule in definitions.items():
            if evaluate_jsonlogic(rule, expanded_query):
                matched_value = cohort_value
                break
        expanded_query[dimension] = matched_value

    return expanded_query


class GeneratedResolver:
    def __init__(self, parsed: dict[str, Any]):
        self.parsed = parsed
        self.defaults = {
            key: spec["value"]
            for key, spec in parsed.get("default-configs", {}).items()
        }
        self.overrides_by_payment_method: dict[str, list[dict[str, Any]]] = defaultdict(list)
        self.overrides_without_payment_method: list[dict[str, Any]] = []

        for override in parsed.get("overrides", []):
            payment_method = override.get("_context_", {}).get("payment_method")
            if isinstance(payment_method, str):
                self.overrides_by_payment_method[payment_method].append(override)
            else:
                self.overrides_without_payment_method.append(override)

    def resolve(self, query: dict[str, str]) -> dict[str, Any]:
        query = query_with_local_cohorts(self.parsed, query)
        result = dict(self.defaults)
        candidates = [
            *self.overrides_by_payment_method.get(
                query.get("payment_method", ""),
                [],
            ),
            *self.overrides_without_payment_method,
        ]
        for override in candidates:
            context = override["_context_"]
            if all(
                dimension in query and value_matches(condition_value, query[dimension])
                for dimension, condition_value in context.items()
            ):
                for key, value in override.items():
                    if key != "_context_":
                        result[key] = value
        return result


def resolve_generated(parsed: dict[str, Any], query: dict[str, str]) -> dict[str, Any]:
    return GeneratedResolver(parsed).resolve(query)


def source_pm_row(model: SourceModel, connector: str, payment_method: str) -> RawPmFilterRow | None:
    connector_rows = model.pm_rows_by_connector.get(connector)
    if connector_rows is not None:
        return connector_rows.get(payment_method)
    return model.default_pm_rows.get(payment_method)


def source_payments_enabled(
    model: SourceModel,
    *,
    connector: str,
    payment_method: str,
    country: str,
    currency: str,
    capture_method: str,
) -> bool:
    row = source_pm_row(model, connector, payment_method)
    if row is None:
        return True

    if row.countries is not None and country != UNSPECIFIED and country not in row.countries:
        return False
    if row.currencies is not None and currency != UNSPECIFIED and currency not in row.currencies:
        return False
    if capture_method in row.blocked_capture_methods:
        return False
    return True


def source_mandate_enabled(
    connectors_by_pm: dict[str, tuple[str, ...]],
    connector: str,
    payment_method: str,
) -> bool:
    return connector in connectors_by_pm.get(payment_method, tuple())


def normalize_query(
    *,
    connector: str,
    payment_method: str,
    country: str = UNSPECIFIED,
    currency: str = UNSPECIFIED,
    capture_method: str = UNSPECIFIED,
) -> dict[str, str]:
    return {
        "capture_method": capture_method,
        "connector": connector,
        "payment_method": payment_method,
        "country": country,
        "currency": currency,
    }


def schema_check(parsed: dict[str, Any]) -> list[str]:
    errors: list[str] = []
    dimensions = parsed.get("dimensions", {})
    default_configs = parsed.get("default-configs", {})

    missing_regular_dimensions = sorted(set(DIMENSION_ORDER) - set(dimensions))
    if missing_regular_dimensions:
        errors.append(
            f"missing regular dimensions: {missing_regular_dimensions}"
        )
    if "country_group" in dimensions:
        errors.append("country_group must not be present in v1")

    expected_configs = {
        "payments_enabled",
        "payments_mandates_enabled",
        "payments_zero_dollar_mandates_enabled",
    }
    if set(default_configs) != expected_configs:
        errors.append(
            f"default-configs must be exactly {sorted(expected_configs)}, got {sorted(default_configs)}"
        )

    enums = {
        dimension: set(spec.get("schema", {}).get("enum", []))
        for dimension, spec in dimensions.items()
    }
    positions = [
        spec.get("position")
        for spec in dimensions.values()
        if isinstance(spec.get("position"), int)
    ]
    if len(positions) != len(set(positions)):
        errors.append("dimension positions must be unique")

    cohort_based_on: dict[str, str] = {}
    for dimension, spec in dimensions.items():
        dimension_type = spec.get("type")
        schema = spec.get("schema", {})

        if dimension in DIMENSION_ORDER:
            if dimension_type is not None:
                errors.append(f"regular dimension {dimension} must not declare type")
            if schema.get("type") != "string":
                errors.append(f"regular dimension {dimension} must be a scalar string")
            continue

        if not (
            isinstance(dimension_type, str)
            and dimension_type.startswith("LOCAL_COHORT:")
        ):
            errors.append(
                f"non-regular dimension {dimension} must be a LOCAL_COHORT"
            )
            continue

        based_on = dimension_type.split(":", 1)[1]
        cohort_based_on[dimension] = based_on
        if based_on not in DIMENSION_ORDER:
            errors.append(
                f"cohort dimension {dimension} must be based on a regular dimension, got {based_on}"
            )
        if schema.get("type") != "string":
            errors.append(f"cohort dimension {dimension} must be a scalar string")
        if set(schema.get("enum", [])) != {COHORT_MATCH_VALUE, COHORT_FALLBACK_VALUE}:
            errors.append(
                f"cohort dimension {dimension} must declare enum {[COHORT_MATCH_VALUE, COHORT_FALLBACK_VALUE]}"
            )
        definitions = schema.get("definitions", {})
        rule = definitions.get(COHORT_MATCH_VALUE)
        if not evaluate_jsonlogic(rule, {based_on: "__schema_check_probe__"}):
            # The probe is intentionally not expected to match most cohorts. This
            # branch only protects the shape below, so inspect it explicitly.
            if not (
                isinstance(rule, dict)
                and isinstance(rule.get("in"), list)
                and len(rule["in"]) == 2
                and rule["in"][0] == {"var": based_on}
                and isinstance(rule["in"][1], list)
            ):
                errors.append(
                    f"cohort dimension {dimension} must use an in-list definition based on {based_on}"
                )
        if based_on in dimensions and isinstance(rule, dict):
            values = rule.get("in", [None, []])[1]
            if isinstance(values, list):
                unknown_values = sorted(set(values) - enums.get(based_on, set()))
                if unknown_values:
                    errors.append(
                        f"cohort dimension {dimension} has values outside {based_on} enum: {unknown_values[:5]}"
                    )

    for index, override in enumerate(parsed.get("overrides", [])):
        context = override.get("_context_", {})
        if not context:
            errors.append(f"override #{index} must contain at least one context dimension")
        if "payment_method" not in context:
            errors.append(
                f"override #{index} must contain payment_method for domain clarity and efficient parity validation"
            )

        for dimension, value in context.items():
            if dimension not in dimensions:
                errors.append(
                    f"override #{index} uses undeclared dimension {dimension}"
                )
                continue
            if isinstance(value, list):
                errors.append(
                    f"override #{index} dimension {dimension} uses a list context value; UI-compatible contexts must be scalar"
                )
                continue
            unknown_values = sorted({value} - enums.get(dimension, set()))
            if unknown_values:
                errors.append(
                    f"override #{index} dimension {dimension} has undeclared values {unknown_values[:5]}"
                )

        for cohort_dimension, based_on in cohort_based_on.items():
            if cohort_dimension in context and based_on in context:
                errors.append(
                    f"override #{index} mixes cohort {cohort_dimension} with its source dimension {based_on}"
                )

        for key in override:
            if key != "_context_" and key not in expected_configs:
                errors.append(f"override #{index} has undeclared config key {key}")

    return errors


def pick_outside(universe: Iterable[str], allowed: Iterable[str]) -> str | None:
    allowed_set = set(allowed) | {UNSPECIFIED}
    for value in sorted(universe):
        if value not in allowed_set:
            return value
    return None


def validate_parity(parsed: dict[str, Any], model: SourceModel) -> tuple[int, list[str]]:
    failures: list[str] = []
    checks = 0
    resolver = GeneratedResolver(parsed)

    all_countries = set(model.countries)
    all_currencies = set(model.currencies)

    raw_effective_rows: list[RawPmFilterRow] = []
    for connector_rows in model.pm_rows_by_connector.values():
        raw_effective_rows.extend(connector_rows.values())

    connectors_without_specific_table = [
        connector
        for connector in model.connector_universe
        if connector not in model.pm_rows_by_connector
    ]
    for connector in connectors_without_specific_table:
        for default_row in model.default_pm_rows.values():
            raw_effective_rows.append(
                RawPmFilterRow(
                    connector=connector,
                    payment_method=default_row.payment_method,
                    countries=default_row.countries,
                    currencies=default_row.currencies,
                    blocked_capture_methods=default_row.blocked_capture_methods,
                    source=default_row.source,
                )
            )

    def check_payment(query: dict[str, str], label: str) -> None:
        nonlocal checks
        checks += 1
        got = resolver.resolve(query)["payments_enabled"]
        want = source_payments_enabled(model, **query)
        if got != want:
            failures.append(f"{label}: payments_enabled got {got}, want {want}, query={query}")

    for row in raw_effective_rows:
        valid_country = row.countries[0] if row.countries else UNSPECIFIED
        valid_currency = row.currencies[0] if row.currencies else UNSPECIFIED

        check_payment(
            normalize_query(
                connector=row.connector,
                payment_method=row.payment_method,
                country=valid_country,
                currency=valid_currency,
                capture_method="automatic",
            ),
            f"{row.source} valid boundary",
        )
        check_payment(
            normalize_query(
                connector=row.connector,
                payment_method=row.payment_method,
                country=UNSPECIFIED,
                currency=UNSPECIFIED,
                capture_method=UNSPECIFIED,
            ),
            f"{row.source} unspecified request values",
        )

        if row.countries is not None:
            denied_country = pick_outside(all_countries, row.countries)
            if denied_country:
                check_payment(
                    normalize_query(
                        connector=row.connector,
                        payment_method=row.payment_method,
                        country=denied_country,
                        currency=valid_currency,
                        capture_method="automatic",
                    ),
                    f"{row.source} denied country",
                )

        if row.currencies is not None:
            denied_currency = pick_outside(all_currencies, row.currencies)
            if denied_currency:
                check_payment(
                    normalize_query(
                        connector=row.connector,
                        payment_method=row.payment_method,
                        country=valid_country,
                        currency=denied_currency,
                        capture_method="automatic",
                    ),
                    f"{row.source} denied currency",
                )

        for blocked_capture in row.blocked_capture_methods:
            check_payment(
                normalize_query(
                    connector=row.connector,
                    payment_method=row.payment_method,
                    country=valid_country,
                    currency=valid_currency,
                    capture_method=blocked_capture,
                ),
                f"{row.source} blocked capture",
            )

    # Connector-specific tables suppress default table lookup for that connector
    # in Hyperswitch's listing filter. If a PM is absent in the connector table,
    # it resolves to true instead of falling back to pm_filters.default.
    for connector, connector_rows in model.pm_rows_by_connector.items():
        missing_default_pms = sorted(set(model.default_pm_rows) - set(connector_rows))
        if not missing_default_pms:
            continue
        payment_method = missing_default_pms[0]
        check_payment(
            normalize_query(connector=connector, payment_method=payment_method),
            f"{connector} missing connector PM does not use default fallback",
        )

    for connector in model.connector_universe:
        for payment_method in model.payment_methods:
            query = normalize_query(connector=connector, payment_method=payment_method)
            result = resolver.resolve(query)
            checks += 2

            want_mandate = source_mandate_enabled(
                model.mandate_connectors_by_pm,
                connector,
                payment_method,
            )
            got_mandate = result["payments_mandates_enabled"]
            if got_mandate != want_mandate:
                failures.append(
                    f"mandate mismatch for {connector}/{payment_method}: got {got_mandate}, want {want_mandate}"
                )

            want_zero = source_mandate_enabled(
                model.zero_mandate_connectors_by_pm,
                connector,
                payment_method,
            )
            got_zero = result["payments_zero_dollar_mandates_enabled"]
            if got_zero != want_zero:
                failures.append(
                    f"zero mandate mismatch for {connector}/{payment_method}: got {got_zero}, want {want_zero}"
                )

    return checks, failures


def parse_generated(text: str) -> dict[str, Any]:
    return tomllib.loads(text)


def validate_generated(text: str, model: SourceModel) -> tuple[int, list[str]]:
    errors: list[str] = []
    try:
        parsed = parse_generated(text)
    except tomllib.TOMLDecodeError as error:
        return 0, [f"TOML parse failed: {error}"]

    errors.extend(schema_check(parsed))
    if errors:
        return 0, errors

    checks, parity_errors = validate_parity(parsed, model)
    errors.extend(parity_errors)
    return checks, errors


def main() -> int:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "--source",
        type=Path,
        default=None,
        help=(
            "Hyperswitch config/development.toml. Defaults to "
            "$HYPERSWITCH_DEV_TOML, then ~/hyperswitch/hyperswitch/config/development.toml."
        ),
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=DEFAULT_OUTPUT,
        help=f"SuperTOML output path (default: {DEFAULT_OUTPUT})",
    )
    parser.add_argument(
        "--validate-only",
        action="store_true",
        help="validate an existing output file without rewriting it",
    )
    parser.add_argument(
        "--skip-code-contract",
        action="store_true",
        help="skip Hyperswitch Rust code contract checks",
    )
    args = parser.parse_args()

    source_path = args.source or discover_source()
    if source_path is None or not source_path.is_file():
        print(
            "ERROR: source config not found. Pass --source /path/to/hyperswitch/config/development.toml",
            file=sys.stderr,
        )
        return 2

    source_path = source_path.resolve()
    hyperswitch_root = hyperswitch_root_from_source(source_path)
    if not args.skip_code_contract:
        code_errors = verify_hyperswitch_code_contract(hyperswitch_root)
        if code_errors:
            print("ERROR: Hyperswitch code contract check failed:", file=sys.stderr)
            for error in code_errors:
                print(f"  - {error}", file=sys.stderr)
            return 1

    model = build_source_model(source_path)

    if args.validate_only:
        if not args.output.is_file():
            print(f"ERROR: output file not found: {args.output}", file=sys.stderr)
            return 2
        text = args.output.read_text()
    else:
        text = emit_supertoml(model)
        args.output.write_text(text)

    checks, errors = validate_generated(text, model)
    if errors:
        print("VALIDATION FAILED:", file=sys.stderr)
        for error in errors[:25]:
            print(f"  - {error}", file=sys.stderr)
        if len(errors) > 25:
            print(f"  ... and {len(errors) - 25} more", file=sys.stderr)
        return 1

    parsed = parse_generated(text)
    print(f"{'Validated' if args.validate_only else 'Wrote'} {args.output}")
    print(f"  source:          {source_path}")
    print(f"  source sha256:   {model.source_sha256}")
    if model.hyperswitch_commit:
        print(f"  hyperswitch git: {model.hyperswitch_commit}")
    print(f"  default-configs: {len(parsed['default-configs'])}")
    print(f"  dimensions:      {len(parsed['dimensions'])}")
    print(f"  gen cohorts:     {len(parsed['dimensions']) - len(DIMENSION_ORDER)}")
    print(f"  overrides:       {len(parsed.get('overrides', []))}")
    print(f"  connectors:      {len(model.connector_universe)}")
    print(f"  payment_methods: {len(model.payment_methods)}")
    print(f"  countries:       {len(model.countries)}")
    print(f"  currencies:      {len(model.currencies)}")
    print(f"  parity checks:   {checks}")
    print("OK")
    return 0


if __name__ == "__main__":
    sys.exit(main())
