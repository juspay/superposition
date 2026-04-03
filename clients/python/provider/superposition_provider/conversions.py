"""
Conversion utilities for FFI and data type transformations.

Handles conversions between:
- Smithy Document types (from SDK) to Python types
- Evaluation contexts to query data dictionaries
- Configuration formats
"""

import json
import logging
from decimal import Decimal
from typing import Any

from smithy_core.documents import Document
from smithy_core.shapes import ShapeType
from superposition_bindings.superposition_client import FfiExperiment, FfiExperimentGroup
from superposition_bindings.superposition_types import GroupType, ExperimentStatusType, Variant, VariantType, Config, \
    Context, DimensionInfo, DimensionType
from superposition_sdk.models import ExperimentStatusType as SDKExperimentStatusType, GroupType as SDKGroupType, \
    ExperimentResponse, ExperimentGroupResponse, GetConfigOutput
from superposition_sdk.models import (
    DimensionType as SDKDimensionType,
    DimensionTypeLOCAL_COHORT,
    DimensionTypeREMOTE_COHORT,
)

logger = logging.getLogger(__name__)

def config_response_to_ffi_config(response: GetConfigOutput) -> Config:
    default_configs: dict[str, str] = {}
    for key, value in response.default_configs.items():
        default_configs[key] = json.dumps(document_to_python_value(value))

    overrides = {}
    for (key, value) in response.overrides.items():
        override = {}
        for (key1, value1) in value.items():
            override[key1] = json.dumps(document_to_python_value(value1))
        overrides[key] = override

    context = []
    for ele in response.contexts:
        condition = {}
        for key, value in ele.condition.items():
            condition[key] = json.dumps(document_to_python_value(value))
        cv = Context(
            id=ele.id,
            priority=ele.priority,
            weight=ele.weight,
            override_with_keys=ele.override_with_keys,
            condition=condition
        )
        context.append(cv)

    dimensions = {}
    for (key, dim_info) in response.dimensions.items():
        dimension = DimensionInfo(
            schema={k: json.dumps(document_to_python_value(v)) for k, v in dim_info.schema.items()},
            position=dim_info.position,
            dimension_type=to_dimension_type(dim_info.dimension_type),
            dependency_graph=dim_info.dependency_graph,
            value_compute_function_name=dim_info.value_compute_function_name,
        )
        dimensions[key] = dimension


    return Config(
        contexts=context,
        overrides=overrides,
        default_configs=default_configs,
        dimensions=dimensions,
    )

def to_dimension_type(sdk_dim_type: SDKDimensionType) -> DimensionType:
    match sdk_dim_type:
        case DimensionTypeLOCAL_COHORT():
            return DimensionType.LOCAL_COHORT(sdk_dim_type.value)

        case DimensionTypeREMOTE_COHORT():
            return DimensionType.REMOTE_COHORT(sdk_dim_type.value)

        case _:
            return DimensionType.REGULAR()

def experiments_to_ffi_experiments(exp_list: list[ExperimentResponse]) -> list[FfiExperiment]:
    trimmed_exp_list = []
    for exp in exp_list:
        condition = {}
        for key, value in exp.context.items():
            condition[key] = json.dumps(document_to_python_value(value))

        variants = []

        for variant in exp.variants:
            variant_type = VariantType.CONTROL if variant.variant_type == "CONTROL" else VariantType.EXPERIMENTAL
            overrides = {
                key: json.dumps(document_to_python_value(value))
                for key, value in variant.overrides.items()
            }
            variants.append(
                Variant(
                    id=variant.id,
                    variant_type=variant_type,
                    context_id=variant.context_id,
                    override_id=variant.override_id,
                    overrides=overrides
                )
            )

        trimmed_exp = FfiExperiment(
            id=exp.id,
            context=condition,
            variants=variants,
            traffic_percentage=exp.traffic_percentage,
            status=to_experiment_status_type(exp.status),
        )

        trimmed_exp_list.append(trimmed_exp)

    return trimmed_exp_list

def exp_grps_to_ffi_exp_grps(exp_grp_list: list[ExperimentGroupResponse]) -> list[FfiExperimentGroup]:
    trimmed_exp_grp_list = []
    for exp_gr in exp_grp_list:
        condition = {}
        for key, value in exp_gr.context.items():
            condition[key] = json.dumps(document_to_python_value(value))

        trimmed_exp_grp = FfiExperimentGroup(
            id=exp_gr.id,
            context=condition,
            member_experiment_ids=exp_gr.member_experiment_ids,
            traffic_percentage=exp_gr.traffic_percentage,
            group_type=to_group_type(exp_gr.group_type),
            buckets=exp_gr.buckets
        )

        trimmed_exp_grp_list.append(trimmed_exp_grp)

    return trimmed_exp_grp_list

def document_to_python_value(doc: Document) -> Any:
    """Recursively unwrap smithy_core.Document into plain Python values."""
    if doc.is_none():
        return None

    match doc.shape_type:
        case ShapeType.BOOLEAN:
            return doc.as_boolean()
        case ShapeType.STRING:
            return doc.as_string()
        case ShapeType.BLOB:
            return doc.as_blob()
        case ShapeType.TIMESTAMP:
            return doc.as_timestamp()
        case ShapeType.BYTE | ShapeType.SHORT | ShapeType.INTEGER | ShapeType.LONG | ShapeType.BIG_INTEGER:
            return doc.as_integer()
        case ShapeType.FLOAT | ShapeType.DOUBLE:
            return doc.as_float()
        case ShapeType.BIG_DECIMAL:
            # Convert Decimal to float for JSON compatibility
            decimal_val = doc.as_decimal()
            return float(decimal_val) if decimal_val is not None else None
        case ShapeType.LIST:
            return [document_to_python_value(e) for e in doc.as_list()]
        case ShapeType.STRUCTURE | ShapeType.UNION | ShapeType.MAP:
            return {
                key: document_to_python_value(value)
                for key, value in doc.as_map().items()
            }
        case _:
            # Fallback to doc.as_value() if unknown shape or primitive
            val = doc.as_value()
            # Handle Decimal in fallback case too
            if isinstance(val, Decimal):
                return float(val)
            return val

def to_group_type(sdk_group_type: str) -> GroupType:
    match sdk_group_type:
        case SDKGroupType.USER_CREATED:
            return GroupType.USER_CREATED
        case _:
            return GroupType.SYSTEM_GENERATED

def to_experiment_status_type(sdk_status_type: str) -> ExperimentStatusType:
    match sdk_status_type:
        case SDKExperimentStatusType.CREATED:
            return ExperimentStatusType.CREATED
        case SDKExperimentStatusType.CONCLUDED:
            return ExperimentStatusType.CONCLUDED
        case SDKExperimentStatusType.INPROGRESS:
            return ExperimentStatusType.INPROGRESS
        case SDKExperimentStatusType.PAUSED:
            return ExperimentStatusType.PAUSED
        case _:
            return ExperimentStatusType.DISCARDED
