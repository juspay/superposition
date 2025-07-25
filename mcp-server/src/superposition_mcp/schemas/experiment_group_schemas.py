"""Pydantic schemas for experiment group management operations."""

from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field


class CreateExperimentGroupSchema(BaseModel):
    """Schema for creating a new experiment group."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    name: str = Field(
        description="Name of the experiment group"
    )
    
    description: Optional[str] = Field(
        default=None,
        description="Description of the experiment group"
    )
    
    created_by: str = Field(
        description="User who created the experiment group"
    )


class GetExperimentGroupSchema(BaseModel):
    """Schema for getting a specific experiment group."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    group_id: str = Field(
        description="Experiment group ID to retrieve"
    )


class ListExperimentGroupsSchema(BaseModel):
    """Schema for listing experiment groups."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    count: Optional[int] = Field(
        default=50,
        ge=1,
        le=1000,
        description="Number of experiment groups to retrieve per page"
    )
    
    page: Optional[int] = Field(
        default=1,
        ge=1,
        description="Page number for pagination"
    )


class UpdateExperimentGroupSchema(BaseModel):
    """Schema for updating an experiment group."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    group_id: str = Field(
        description="Experiment group ID to update"
    )
    
    name: Optional[str] = Field(
        default=None,
        description="Updated name of the experiment group"
    )
    
    description: Optional[str] = Field(
        default=None,
        description="Updated description of the experiment group"
    )


class DeleteExperimentGroupSchema(BaseModel):
    """Schema for deleting an experiment group."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    group_id: str = Field(
        description="Experiment group ID to delete"
    )


class AddMembersToGroupSchema(BaseModel):
    """Schema for adding members to an experiment group."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    group_id: str = Field(
        description="Experiment group ID to add members to"
    )
    
    member_ids: List[str] = Field(
        description="List of experiment IDs to add to the group"
    )


class RemoveMembersFromGroupSchema(BaseModel):
    """Schema for removing members from an experiment group."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    group_id: str = Field(
        description="Experiment group ID to remove members from"
    )
    
    member_ids: List[str] = Field(
        description="List of experiment IDs to remove from the group"
    )