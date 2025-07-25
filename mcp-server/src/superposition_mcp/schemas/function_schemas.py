"""Pydantic schemas for function-related operations."""

from typing import Any, Dict, Optional

from pydantic import BaseModel, Field


class CreateFunctionSchema(BaseModel):
    """Schema for creating a function."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    function_name: str = Field(
        description="Name of the function"
    )
    
    runtime: str = Field(
        description="Runtime for the function (e.g., 'javascript', 'python')"
    )
    
    code: str = Field(
        description="Function code"
    )
    
    description: Optional[str] = Field(
        default=None,
        description="Optional description of the function"
    )


class GetFunctionSchema(BaseModel):
    """Schema for getting a function."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    function_name: str = Field(
        description="Name of the function to retrieve"
    )


class ListFunctionSchema(BaseModel):
    """Schema for listing functions."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )


class UpdateFunctionSchema(BaseModel):
    """Schema for updating a function."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    function_name: str = Field(
        description="Name of the function to update"
    )
    
    runtime: Optional[str] = Field(
        default=None,
        description="Runtime for the function (e.g., 'javascript', 'python')"
    )
    
    code: Optional[str] = Field(
        default=None,
        description="Function code"
    )
    
    description: Optional[str] = Field(
        default=None,
        description="Description of the function"
    )


class DeleteFunctionSchema(BaseModel):
    """Schema for deleting a function."""
    
    workspace_id: Optional[str] = Field(
        default=None,
        description="Workspace ID. If not provided, uses default from server config."
    )
    
    org_id: Optional[str] = Field(
        default=None,
        description="Organization ID. If not provided, uses default from server config."
    )
    
    function_name: str = Field(
        description="Name of the function to delete"
    )