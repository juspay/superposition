"""Pydantic schemas for experiment-related operations."""

from typing import Any, Dict, List, Optional
from datetime import datetime

from pydantic import BaseModel, Field


class VariantSchema(BaseModel):
    """Schema for experiment variant."""
    
    id: str = Field(
        description="Unique identifier for this variant"
    )
    
    variant_type: str = Field(
        description="Type of variant: CONTROL or EXPERIMENTAL"
    )
    
    overrides: Dict[str, Any] = Field(
        description="Configuration overrides for this variant"
    )
    
    context_id: Optional[str] = Field(
        default=None,
        description="Context ID associated with this variant"
    )
    
    override_id: Optional[str] = Field(
        default=None,
        description="Override ID associated with this variant"
    )


class CreateExperimentSchema(BaseModel):
    """Schema for creating a new experiment."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    name: str = Field(
        description="Name of the experiment"
    )
    
    experiment_type: Optional[str] = Field(
        default="DEFAULT",
        description="Type of experiment: DEFAULT or DELETE_OVERRIDES"
    )
    
    context: Dict[str, Any] = Field(
        description="Context condition for the experiment"
    )
    
    variants: List[VariantSchema] = Field(
        description="List of experiment variants"
    )
    
    description: str = Field(
        description="Description of the experiment"
    )
    
    change_reason: str = Field(
        description="Reason for creating this experiment (required for audit)"
    )
    
    metrics: Optional[Dict[str, Any]] = Field(
        default=None,
        description="Metrics configuration for the experiment"
    )
    
    experiment_group_id: Optional[str] = Field(
        default=None,
        description="ID of the experiment group this experiment belongs to"
    )


class GetExperimentSchema(BaseModel):
    """Schema for getting a specific experiment by ID."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    experiment_id: str = Field(
        description="Unique identifier of the experiment to retrieve"
    )


class UpdateExperimentSchema(BaseModel):
    """Schema for updating experiment overrides."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    experiment_id: str = Field(
        description="Unique identifier of the experiment to update"
    )
    
    variants: List[VariantSchema] = Field(
        description="Updated list of experiment variants"
    )
    
    description: Optional[str] = Field(
        default=None,
        description="Updated description for the experiment"
    )
    
    change_reason: str = Field(
        description="Reason for updating this experiment (required for audit)"
    )
    
    metrics: Optional[Dict[str, Any]] = Field(
        default=None,
        description="Updated metrics configuration"
    )
    
    experiment_group_id: Optional[str] = Field(
        default=None,
        description="Updated experiment group ID"
    )


class ConcludeExperimentSchema(BaseModel):
    """Schema for concluding an experiment."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    experiment_id: str = Field(
        description="Unique identifier of the experiment to conclude"
    )
    
    chosen_variant: str = Field(
        description="ID of the winning variant to promote"
    )
    
    description: Optional[str] = Field(
        default=None,
        description="Conclusion notes for the experiment"
    )
    
    change_reason: str = Field(
        description="Reason for concluding this experiment (required for audit)"
    )


class DiscardExperimentSchema(BaseModel):
    """Schema for discarding an experiment."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    experiment_id: str = Field(
        description="Unique identifier of the experiment to discard"
    )
    
    change_reason: str = Field(
        description="Reason for discarding this experiment (required for audit)"
    )


class RampExperimentSchema(BaseModel):
    """Schema for ramping experiment traffic."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    experiment_id: str = Field(
        description="Unique identifier of the experiment to ramp"
    )
    
    traffic_percentage: int = Field(
        ge=0,
        le=100,
        description="New traffic percentage for the experiment"
    )
    
    change_reason: str = Field(
        description="Reason for ramping this experiment (required for audit)"
    )


class PauseExperimentSchema(BaseModel):
    """Schema for pausing an experiment."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    experiment_id: str = Field(
        description="Unique identifier of the experiment to pause"
    )
    
    change_reason: str = Field(
        description="Reason for pausing this experiment (required for audit)"
    )


class ResumeExperimentSchema(BaseModel):
    """Schema for resuming an experiment."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    experiment_id: str = Field(
        description="Unique identifier of the experiment to resume"
    )
    
    change_reason: str = Field(
        description="Reason for resuming this experiment (required for audit)"
    )


class ListExperimentsSchema(BaseModel):
    """Schema for listing experiments with filtering and pagination."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    page: Optional[int] = Field(
        default=1,
        ge=1,
        description="Page number for pagination"
    )
    
    count: Optional[int] = Field(
        default=50,
        ge=1,
        le=1000,
        description="Number of experiments to retrieve per page"
    )
    
    all: Optional[bool] = Field(
        default=False,
        description="Retrieve all experiments (ignores pagination)"
    )
    
    status: Optional[str] = Field(
        default=None,
        description="Filter by experiment status: CREATED, INPROGRESS, CONCLUDED, DISCARDED, PAUSED"
    )
    
    from_date: Optional[datetime] = Field(
        default=None,
        description="Filter experiments created after this date"
    )
    
    to_date: Optional[datetime] = Field(
        default=None,
        description="Filter experiments created before this date"
    )
    
    experiment_name: Optional[str] = Field(
        default=None,
        description="Filter by experiment name (partial match)"
    )
    
    experiment_ids: Optional[str] = Field(
        default=None,
        description="Comma-separated list of experiment IDs to filter"
    )
    
    experiment_group_ids: Optional[str] = Field(
        default=None,
        description="Comma-separated list of experiment group IDs to filter"
    )
    
    created_by: Optional[str] = Field(
        default=None,
        description="Filter by experiment creator"
    )
    
    sort_on: Optional[str] = Field(
        default=None,
        description="Field to sort by: last_modified_at or created_at"
    )
    
    sort_by: Optional[str] = Field(
        default="ASC",
        description="Sort direction: ASC or DESC"
    )


class ApplicableVariantsSchema(BaseModel):
    """Schema for getting applicable experiment variants for a context."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    context: Dict[str, Any] = Field(
        description="Context to evaluate for applicable variants"
    )
    
    toss: int = Field(
        ge=0,
        le=100,
        description="Random toss value for determining variant assignment"
    )