"""Pydantic schemas for dimension management operations."""

from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field


class CreateDimensionSchema(BaseModel):
    """Schema for creating a new dimension."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    dimension: str = Field(
        description="Name of the dimension"
    )
    
    position: Optional[int] = Field(
        default=None,
        description="Position/priority of the dimension"
    )
    
    created_by: str = Field(
        description="User who created the dimension"
    )
    
    json_schema: Optional[Dict[str, Any]] = Field(
        default=None,
        description="JSON schema for validating dimension values",
        alias="schema"
    )
    
    function_name: Optional[str] = Field(
        default=None,
        description="Name of the function associated with this dimension"
    )


class GetDimensionSchema(BaseModel):
    """Schema for getting a specific dimension."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    dimension_name: str = Field(
        description="Name of the dimension to retrieve"
    )


class ListDimensionsSchema(BaseModel):
    """Schema for listing dimensions."""
    
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
        description="Number of dimensions to retrieve per page"
    )
    
    page: Optional[int] = Field(
        default=1,
        ge=1,
        description="Page number for pagination"
    )


class UpdateDimensionSchema(BaseModel):
    """Schema for updating a dimension."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    dimension_name: str = Field(
        description="Name of the dimension to update"
    )
    
    position: Optional[int] = Field(
        default=None,
        description="New position/priority of the dimension"
    )
    
    json_schema: Optional[Dict[str, Any]] = Field(
        default=None,
        description="Updated JSON schema for validating dimension values",
        alias="schema"
    )
    
    function_name: Optional[str] = Field(
        default=None,
        description="Updated function name associated with this dimension"
    )


class DeleteDimensionSchema(BaseModel):
    """Schema for deleting a dimension."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    dimension_name: str = Field(
        description="Name of the dimension to delete"
    )