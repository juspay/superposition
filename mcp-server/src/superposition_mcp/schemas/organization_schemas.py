"""Pydantic schemas for organization-related operations."""

from typing import Any, Dict, Optional

from pydantic import BaseModel, Field


class CreateOrganizationSchema(BaseModel):
    """Schema for creating an organization."""
    
    org_name: str = Field(
        description="Name of the organization"
    )
    
    description: Optional[str] = Field(
        default=None,
        description="Description of the organization"
    )


class GetOrganizationSchema(BaseModel):
    """Schema for getting an organization."""
    
    org_id: str = Field(
        description="Organization ID to retrieve"
    )


class ListOrganizationSchema(BaseModel):
    """Schema for listing organizations."""
    
    count: Optional[int] = Field(
        default=50,
        ge=1,
        le=1000,
        description="Number of organizations to retrieve per page"
    )
    
    page: Optional[int] = Field(
        default=1,
        ge=1,
        description="Page number for pagination"
    )


class UpdateOrganizationSchema(BaseModel):
    """Schema for updating an organization."""
    
    org_id: str = Field(
        description="Organization ID to update"
    )
    
    org_name: Optional[str] = Field(
        default=None,
        description="Updated name of the organization"
    )
    
    description: Optional[str] = Field(
        default=None,
        description="Updated description of the organization"
    )