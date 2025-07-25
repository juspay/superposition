"""Pydantic schemas for configuration-related operations."""

from typing import Any, Dict, Optional

from pydantic import BaseModel, Field


class GetConfigSchema(BaseModel):
    """Schema for getting configuration with context."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    prefix: Optional[str] = Field(
        default=None,
        description="Configuration key prefix to filter results"
    )
    
    version: Optional[str] = Field(
        default=None,
        description="Specific configuration version to retrieve"
    )
    
    context: Optional[Dict[str, Any]] = Field(
        default=None,
        description="Context map for resolving configuration (dimension -> value pairs)"
    )


class GetResolvedConfigSchema(BaseModel):
    """Schema for getting resolved configuration."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    prefix: Optional[str] = Field(
        default=None,
        description="Configuration key prefix to filter results"
    )
    
    version: Optional[str] = Field(
        default=None,
        description="Specific configuration version to retrieve"
    )
    
    context: Optional[Dict[str, Any]] = Field(
        default=None,
        description="Context map for resolving configuration (dimension -> value pairs)"
    )
    
    context_id: Optional[str] = Field(
        default=None,
        description="Specific context ID to use for resolution"
    )
    
    show_reasoning: Optional[bool] = Field(
        default=False,
        description="Include reasoning information in the response"
    )
    
    merge_strategy: Optional[str] = Field(
        default="MERGE",
        description="Strategy for merging configurations: MERGE or REPLACE"
    )


class ListConfigVersionsSchema(BaseModel):
    """Schema for listing configuration versions."""
    
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
        description="Number of versions to retrieve per page"
    )
    
    page: Optional[int] = Field(
        default=1,
        ge=1,
        description="Page number for pagination"
    )