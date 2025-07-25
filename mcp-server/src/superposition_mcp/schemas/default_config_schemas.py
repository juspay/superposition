"""Pydantic schemas for default configuration management operations."""

from typing import Any, Dict, Optional

from pydantic import BaseModel, Field


class CreateDefaultConfigSchema(BaseModel):
    """Schema for creating a new default configuration."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    key: str = Field(
        description="Configuration key"
    )
    
    value: Any = Field(
        description="Configuration value (can be any JSON-serializable type)"
    )
    
    json_schema: Optional[Dict[str, Any]] = Field(
        default=None,
        description="JSON schema for validating the configuration value",
        alias="schema"
    )
    
    description: Optional[str] = Field(
        default=None,
        description="Description of what this configuration key controls"
    )
    
    created_by: str = Field(
        description="User who created this configuration"
    )


class ListDefaultConfigsSchema(BaseModel):
    """Schema for listing default configurations."""
    
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
        description="Number of configurations to retrieve per page"
    )
    
    page: Optional[int] = Field(
        default=1,
        ge=1,
        description="Page number for pagination"
    )
    
    prefix: Optional[str] = Field(
        default=None,
        description="Filter configurations by key prefix"
    )


class UpdateDefaultConfigSchema(BaseModel):
    """Schema for updating a default configuration."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    key: str = Field(
        description="Configuration key to update"
    )
    
    value: Any = Field(
        description="New configuration value (can be any JSON-serializable type)"
    )
    
    json_schema: Optional[Dict[str, Any]] = Field(
        default=None,
        description="Updated JSON schema for validating the configuration value",
        alias="schema"
    )
    
    description: Optional[str] = Field(
        default=None,
        description="Updated description of what this configuration key controls"
    )


class DeleteDefaultConfigSchema(BaseModel):
    """Schema for deleting a default configuration."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    config_key: str = Field(
        description="Configuration key to delete"
    )