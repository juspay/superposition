"""Pydantic schemas for context-related operations."""

from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field


class CreateContextSchema(BaseModel):
    """Schema for creating a new context."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    context: Dict[str, Any] = Field(
        description="Context condition as a map of dimension-value pairs"
    )
    
    override: Dict[str, Any] = Field(
        description="Configuration overrides to apply for this context"
    )
    
    description: Optional[str] = Field(
        default=None,
        description="Human-readable description of this context"
    )
    
    change_reason: str = Field(
        description="Reason for creating this context (required for audit)"
    )
    
    config_tags: Optional[str] = Field(
        default=None,
        description="Tags to associate with this context"
    )


class GetContextSchema(BaseModel):
    """Schema for getting a specific context by ID."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    context_id: str = Field(
        description="Unique identifier of the context to retrieve"
    )


class UpdateContextSchema(BaseModel):
    """Schema for updating context overrides."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    context_id: Optional[str] = Field(
        default=None,
        description="Context ID to update (use either this or context)"
    )
    
    context: Optional[Dict[str, Any]] = Field(
        default=None,
        description="Context condition to update (use either this or context_id)"
    )
    
    override: Dict[str, Any] = Field(
        description="New configuration overrides"
    )
    
    description: Optional[str] = Field(
        default=None,
        description="Updated description for this context"
    )
    
    change_reason: str = Field(
        description="Reason for updating this context (required for audit)"
    )
    
    config_tags: Optional[str] = Field(
        default=None,
        description="Tags to associate with this context update"
    )


class DeleteContextSchema(BaseModel):
    """Schema for deleting a context."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    context_id: str = Field(
        description="Unique identifier of the context to delete"
    )
    
    config_tags: Optional[str] = Field(
        default=None,
        description="Tags to associate with this deletion"
    )


class MoveContextSchema(BaseModel):
    """Schema for moving a context (changing its condition)."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    context_id: str = Field(
        description="Unique identifier of the context to move"
    )
    
    context: Dict[str, Any] = Field(
        description="New context condition as a map of dimension-value pairs"
    )
    
    description: Optional[str] = Field(
        default=None,
        description="Updated description for this context"
    )
    
    change_reason: str = Field(
        description="Reason for moving this context (required for audit)"
    )


class ListContextsSchema(BaseModel):
    """Schema for listing contexts with filtering and pagination."""
    
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
        description="Number of contexts to retrieve per page"
    )
    
    prefix: Optional[str] = Field(
        default=None,
        description="Filter contexts by configuration key prefix"
    )
    
    sort_on: Optional[str] = Field(
        default=None,
        description="Field to sort by: last_modified_at, created_at, or weight"
    )
    
    sort_by: Optional[str] = Field(
        default="ASC",
        description="Sort direction: ASC or DESC"
    )
    
    created_by: Optional[str] = Field(
        default=None,
        description="Filter by context creator"
    )
    
    last_modified_by: Optional[str] = Field(
        default=None,
        description="Filter by who last modified the context"
    )
    
    plaintext: Optional[str] = Field(
        default=None,
        description="Search term for plaintext search in contexts"
    )