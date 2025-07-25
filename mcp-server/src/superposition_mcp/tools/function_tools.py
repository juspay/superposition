"""Function management tools for Superposition MCP server."""

from typing import Any, Dict

from mcp.types import TextContent

from ..schemas.function_schemas import (
    CreateFunctionSchema,
    GetFunctionSchema,
    ListFunctionSchema,
    UpdateFunctionSchema,
    DeleteFunctionSchema,
)
from .base import BaseTool


class CreateFunctionTool(BaseTool):
    """Tool for creating a function."""
    
    @property
    def name(self) -> str:
        return "create_function"
    
    @property
    def description(self) -> str:
        return """Create a new function in Superposition.
        
        Functions are custom code snippets that can be used in configuration values
        for dynamic evaluation and processing.
        
        Example:
        {
            "function_name": "calculate_discount",
            "runtime": "javascript",
            "code": "function calculate_discount(price, user_type) { return user_type === 'premium' ? price * 0.8 : price * 0.9; }",
            "description": "Calculate discount based on user type"
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
                "function_name": {
                    "type": "string",
                    "description": "Name of the function"
                },
                "runtime": {
                    "type": "string",
                    "description": "Runtime for the function (e.g., 'javascript', 'python')"
                },
                "code": {
                    "type": "string",
                    "description": "Function code"
                },
                "description": {
                    "type": "string",
                    "description": "Optional description of the function"
                }
            },
            "required": ["function_name", "runtime", "code"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self) -> type[CreateFunctionSchema]:
        return CreateFunctionSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the create function operation."""
        inputs = self.validate_input(arguments)
        
        result = await self.client.create_function(
            function_name=inputs.function_name,
            runtime=inputs.runtime,
            code=inputs.code,
            description=inputs.description,
            workspace_id=inputs.workspace_id,
            org_id=inputs.org_id
        )
        
        return self.format_success_response(result)


class GetFunctionTool(BaseTool):
    """Tool for getting a function."""
    
    @property
    def name(self) -> str:
        return "get_function"
    
    @property
    def description(self) -> str:
        return """Get a function from Superposition.
        
        Retrieves the details of a specific function including its code,
        runtime, and metadata.
        
        Example:
        {
            "function_name": "calculate_discount"
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
                "function_name": {
                    "type": "string",
                    "description": "Name of the function to retrieve"
                }
            },
            "required": ["function_name"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self) -> type[GetFunctionSchema]:
        return GetFunctionSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the get function operation."""
        inputs = self.validate_input(arguments)
        
        result = await self.client.get_function(
            function_name=inputs.function_name,
            workspace_id=inputs.workspace_id,
            org_id=inputs.org_id
        )
        
        return self.format_success_response(result)


class ListFunctionTool(BaseTool):
    """Tool for listing functions."""
    
    @property
    def name(self) -> str:
        return "list_functions"
    
    @property
    def description(self) -> str:
        return """List all functions in Superposition.
        
        Retrieves a list of all functions with their metadata.
        
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
    def schema_class(self) -> type[ListFunctionSchema]:
        return ListFunctionSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the list functions operation."""
        inputs = self.validate_input(arguments)
        
        result = await self.client.list_function(
            workspace_id=inputs.workspace_id,
            org_id=inputs.org_id
        )
        
        return self.format_success_response(result)


class UpdateFunctionTool(BaseTool):
    """Tool for updating a function."""
    
    @property
    def name(self) -> str:
        return "update_function"
    
    @property
    def description(self) -> str:
        return """Update an existing function in Superposition.
        
        Updates the code, runtime, or description of an existing function.
        Only provided fields will be updated.
        
        Example:
        {
            "function_name": "calculate_discount",
            "code": "function calculate_discount(price, user_type) { return user_type === 'premium' ? price * 0.7 : price * 0.9; }",
            "description": "Updated discount calculation"
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
                "function_name": {
                    "type": "string",
                    "description": "Name of the function to update"
                },
                "runtime": {
                    "type": "string",
                    "description": "Runtime for the function (e.g., 'javascript', 'python')"
                },
                "code": {
                    "type": "string",
                    "description": "Function code"
                },
                "description": {
                    "type": "string",
                    "description": "Description of the function"
                }
            },
            "required": ["function_name"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self) -> type[UpdateFunctionSchema]:
        return UpdateFunctionSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the update function operation."""
        inputs = self.validate_input(arguments)
        
        result = await self.client.update_function(
            function_name=inputs.function_name,
            runtime=inputs.runtime,
            code=inputs.code,
            description=inputs.description,
            workspace_id=inputs.workspace_id,
            org_id=inputs.org_id
        )
        
        return self.format_success_response(result)


class DeleteFunctionTool(BaseTool):
    """Tool for deleting a function."""
    
    @property
    def name(self) -> str:
        return "delete_function"
    
    @property
    def description(self) -> str:
        return """Delete a function from Superposition.
        
        Permanently removes a function and all its associated data.
        This operation cannot be undone.
        
        Example:
        {
            "function_name": "calculate_discount"
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
                "function_name": {
                    "type": "string",
                    "description": "Name of the function to delete"
                }
            },
            "required": ["function_name"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self) -> type[DeleteFunctionSchema]:
        return DeleteFunctionSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the delete function operation."""
        inputs = self.validate_input(arguments)
        
        result = await self.client.delete_function(
            function_name=inputs.function_name,
            workspace_id=inputs.workspace_id,
            org_id=inputs.org_id
        )
        
        return self.format_success_response(result)