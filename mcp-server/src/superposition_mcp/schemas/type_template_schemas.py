"""Pydantic schemas for type template-related operations."""

from typing import Any, Dict, Optional

from pydantic import BaseModel, Field


class CreateTypeTemplateSchema(BaseModel):
    """Schema for creating a type template."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    type_name: str = Field(
        description="Name of the type template"
    )
    
    type_schema: Dict[str, Any] = Field(
        description="JSON schema definition for the type template"
    )


class GetTypeTemplateSchema(BaseModel):
    """Schema for getting type templates list."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )


class UpdateTypeTemplateSchema(BaseModel):
    """Schema for updating a type template."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    type_name: str = Field(
        description="Name of the type template to update"
    )
    
    type_schema: Dict[str, Any] = Field(
        description="Updated JSON schema definition for the type template"
    )


class DeleteTypeTemplateSchema(BaseModel):
    """Schema for deleting a type template."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    type_name: str = Field(
        description="Name of the type template to delete"
    )