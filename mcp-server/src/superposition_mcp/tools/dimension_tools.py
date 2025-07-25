"""Dimension management tools for Superposition MCP server."""

from typing import Any, Dict

from mcp.types import TextContent

from ..schemas.dimension_schemas import (
    CreateDimensionSchema,
    GetDimensionSchema,
    ListDimensionsSchema,
    UpdateDimensionSchema,
    DeleteDimensionSchema
)
from .base import BaseTool


class CreateDimensionTool(BaseTool):
    """Tool for creating new dimensions."""
    
    @property
    def name(self) -> str:
        return "create_dimension"
    
    @property
    def description(self) -> str:
        return """Create a new dimension in Superposition.
        
        Dimensions are the building blocks of context-aware configuration. They define
        the criteria (like user_type, region, platform) that can be used to create
        conditional configuration overrides.
        
        Examples:
        - User type dimension: {"dimension": "user_type", "created_by": "admin"}
        - Region dimension: {"dimension": "region", "position": 1, "created_by": "admin"}
        - Platform with schema: {
            "dimension": "platform", 
            "created_by": "admin",
            "schema": {"type": "string", "enum": ["web", "mobile", "api"]}
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
                "dimension": {
                    "type": "string",
                    "description": "Name of the dimension"
                },
                "position": {
                    "type": "integer",
                    "description": "Position/priority of the dimension"
                },
                "created_by": {
                    "type": "string",
                    "description": "User who created the dimension"
                },
                "schema": {
                    "type": "object",
                    "description": "JSON schema for validating dimension values",
                    "additionalProperties": True
                },
                "function_name": {
                    "type": "string",
                    "description": "Name of the function associated with this dimension"
                }
            },
            "required": ["dimension", "created_by"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return CreateDimensionSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the create dimension operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.create_dimension(
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            dimension=validated_input.dimension,
            position=validated_input.position,
            created_by=validated_input.created_by,
            schema=validated_input.json_schema,
            function_name=validated_input.function_name
        )
        
        return self.format_success_response(result)


class GetDimensionTool(BaseTool):
    """Tool for retrieving a specific dimension."""
    
    @property
    def name(self) -> str:
        return "get_dimension"
    
    @property
    def description(self) -> str:
        return """Get details of a specific dimension by name.
        
        Retrieves comprehensive information about a dimension including its schema,
        position, associated functions, and metadata.
        
        Examples:
        - Get user_type dimension: {"dimension_name": "user_type"}
        - Get from specific workspace: {"dimension_name": "region", "workspace_id": "prod"}
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
                "dimension_name": {
                    "type": "string",
                    "description": "Name of the dimension to retrieve"
                }
            },
            "required": ["dimension_name"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return GetDimensionSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the get dimension operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.get_dimension(
            dimension_name=validated_input.dimension_name,
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id
        )
        
        return self.format_success_response(result)


class ListDimensionsTool(BaseTool):
    """Tool for listing dimensions."""
    
    @property
    def name(self) -> str:
        return "list_dimensions"
    
    @property
    def description(self) -> str:
        return """List all dimensions in a workspace with pagination support.
        
        Returns a paginated list of dimensions showing their names, positions,
        schemas, and associated functions. Useful for understanding the available
        context criteria for configuration overrides.
        
        Examples:
        - List all dimensions: {}
        - Paginated: {"page": 2, "count": 10}
        - Large list: {"count": 100}
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
                "count": {
                    "type": "integer",
                    "minimum": 1,
                    "maximum": 1000,
                    "description": "Number of dimensions to retrieve per page (default: 50)"
                },
                "page": {
                    "type": "integer",
                    "minimum": 1,
                    "description": "Page number for pagination (default: 1)"
                }
            },
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return ListDimensionsSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the list dimensions operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.list_dimensions(
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            count=validated_input.count,
            page=validated_input.page
        )
        
        return self.format_success_response(result)


class UpdateDimensionTool(BaseTool):
    """Tool for updating dimensions."""
    
    @property
    def name(self) -> str:
        return "update_dimension"
    
    @property
    def description(self) -> str:
        return """Update an existing dimension's properties.
        
        Allows updating the dimension's position, schema, and associated function.
        The dimension name itself cannot be changed - use delete and recreate for that.
        
        Examples:
        - Update position: {"dimension_name": "user_type", "position": 5}
        - Update schema: {"dimension_name": "platform", "schema": {"type": "string", "enum": ["web", "mobile"]}}
        - Update function: {"dimension_name": "region", "function_name": "region_validator"}
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
                "dimension_name": {
                    "type": "string",
                    "description": "Name of the dimension to update"
                },
                "position": {
                    "type": "integer",
                    "description": "New position/priority of the dimension"
                },
                "schema": {
                    "type": "object",
                    "description": "Updated JSON schema for validating dimension values",
                    "additionalProperties": True
                },
                "function_name": {
                    "type": "string",
                    "description": "Updated function name associated with this dimension"
                }
            },
            "required": ["dimension_name"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return UpdateDimensionSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the update dimension operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.update_dimension(
            dimension_name=validated_input.dimension_name,
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            position=validated_input.position,
            schema=validated_input.json_schema,
            function_name=validated_input.function_name
        )
        
        return self.format_success_response(result)


class DeleteDimensionTool(BaseTool):
    """Tool for deleting dimensions."""
    
    @property
    def name(self) -> str:
        return "delete_dimension"
    
    @property
    def description(self) -> str:
        return """Delete a dimension from the workspace.
        
        ⚠️  WARNING: Deleting a dimension will affect all contexts and experiments
        that use this dimension. This operation cannot be undone.
        
        Examples:
        - Delete dimension: {"dimension_name": "old_feature_flag"}
        - Delete from specific workspace: {"dimension_name": "temp_dimension", "workspace_id": "test"}
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
                "dimension_name": {
                    "type": "string",
                    "description": "Name of the dimension to delete"
                }
            },
            "required": ["dimension_name"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return DeleteDimensionSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the delete dimension operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.delete_dimension(
            dimension_name=validated_input.dimension_name,
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id
        )
        
        return self.format_success_response(result)