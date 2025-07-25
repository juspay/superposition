"""Type template management tools for Superposition MCP server."""

from typing import Any, Dict

from mcp.types import TextContent

from ..schemas.type_template_schemas import (
    CreateTypeTemplateSchema,
    GetTypeTemplateSchema,
    UpdateTypeTemplateSchema,
    DeleteTypeTemplateSchema,
)
from .base import BaseTool


class CreateTypeTemplateTool(BaseTool):
    """Tool for creating a type template."""
    
    @property
    def name(self) -> str:
        return "create_type_template"
    
    @property
    def description(self) -> str:
        return """Create a new type template in Superposition.
        
        Type templates define custom data types and validation schemas
        that can be used for configuration values.
        
        Example:
        {
            "type_name": "user_profile",
            "type_schema": {
                "type": "object",
                "properties": {
                    "name": {"type": "string"},
                    "age": {"type": "integer", "minimum": 0},
                    "email": {"type": "string", "format": "email"}
                },
                "required": ["name", "email"]
            }
        }
        """
    
    @property
    def input_schema(self) -> Dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "workspace_id": {
                    "type": "string",
                    "description": "Workspace ID (optional, uses default if not specified)"
                },
                "org_id": {
                    "type": "string",
                    "description": "Organization ID (optional, uses default if not specified)"
                },
                "type_name": {
                    "type": "string",
                    "description": "Name of the type template"
                },
                "type_schema": {
                    "type": "object",
                    "description": "JSON schema definition for the type template"
                }
            },
            "required": ["type_name", "type_schema"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self) -> type[CreateTypeTemplateSchema]:
        return CreateTypeTemplateSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the create type template operation."""
        inputs = self.validate_input(arguments)
        
        result = await self.client.create_type_templates(
            type_name=inputs.type_name,
            type_schema=inputs.type_schema,
            workspace_id=inputs.workspace_id,
            org_id=inputs.org_id
        )
        
        return self.format_success_response(result)


class GetTypeTemplateListTool(BaseTool):
    """Tool for getting the list of type templates."""
    
    @property
    def name(self) -> str:
        return "get_type_templates_list"
    
    @property
    def description(self) -> str:
        return """Get the list of all type templates in Superposition.
        
        Retrieves all custom type templates with their schemas and metadata.
        
        Example:
        {}
        """
    
    @property
    def input_schema(self) -> Dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "workspace_id": {
                    "type": "string",
                    "description": "Workspace ID (optional, uses default if not specified)"
                },
                "org_id": {
                    "type": "string",
                    "description": "Organization ID (optional, uses default if not specified)"
                }
            },
            "additionalProperties": False
        }
    
    @property
    def schema_class(self) -> type[GetTypeTemplateSchema]:
        return GetTypeTemplateSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the get type templates list operation."""
        inputs = self.validate_input(arguments)
        
        result = await self.client.get_type_templates_list(
            workspace_id=inputs.workspace_id,
            org_id=inputs.org_id
        )
        
        return self.format_success_response(result)


class UpdateTypeTemplateTool(BaseTool):
    """Tool for updating a type template."""
    
    @property
    def name(self) -> str:
        return "update_type_template"
    
    @property
    def description(self) -> str:
        return """Update an existing type template in Superposition.
        
        Updates the schema definition of an existing type template.
        
        Example:
        {
            "type_name": "user_profile",
            "type_schema": {
                "type": "object",
                "properties": {
                    "name": {"type": "string"},
                    "age": {"type": "integer", "minimum": 0, "maximum": 120},
                    "email": {"type": "string", "format": "email"},
                    "phone": {"type": "string"}
                },
                "required": ["name", "email"]
            }
        }
        """
    
    @property
    def input_schema(self) -> Dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "workspace_id": {
                    "type": "string",
                    "description": "Workspace ID (optional, uses default if not specified)"
                },
                "org_id": {
                    "type": "string",
                    "description": "Organization ID (optional, uses default if not specified)"
                },
                "type_name": {
                    "type": "string",
                    "description": "Name of the type template to update"
                },
                "type_schema": {
                    "type": "object",
                    "description": "Updated JSON schema definition for the type template"
                }
            },
            "required": ["type_name", "type_schema"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self) -> type[UpdateTypeTemplateSchema]:
        return UpdateTypeTemplateSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the update type template operation."""
        inputs = self.validate_input(arguments)
        
        result = await self.client.update_type_templates(
            type_name=inputs.type_name,
            type_schema=inputs.type_schema,
            workspace_id=inputs.workspace_id,
            org_id=inputs.org_id
        )
        
        return self.format_success_response(result)


class DeleteTypeTemplateTool(BaseTool):
    """Tool for deleting a type template."""
    
    @property
    def name(self) -> str:
        return "delete_type_template"
    
    @property
    def description(self) -> str:
        return """Delete a type template from Superposition.
        
        Permanently removes a type template and all its associated data.
        This operation cannot be undone.
        
        Example:
        {
            "type_name": "user_profile"
        }
        """
    
    @property
    def input_schema(self) -> Dict[str, Any]:
        return {
            "type": "object",
            "properties": {
                "workspace_id": {
                    "type": "string",
                    "description": "Workspace ID (optional, uses default if not specified)"
                },
                "org_id": {
                    "type": "string",
                    "description": "Organization ID (optional, uses default if not specified)"
                },
                "type_name": {
                    "type": "string",
                    "description": "Name of the type template to delete"
                }
            },
            "required": ["type_name"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self) -> type[DeleteTypeTemplateSchema]:
        return DeleteTypeTemplateSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the delete type template operation."""
        inputs = self.validate_input(arguments)
        
        result = await self.client.delete_type_templates(
            type_name=inputs.type_name,
            workspace_id=inputs.workspace_id,
            org_id=inputs.org_id
        )
        
        return self.format_success_response(result)