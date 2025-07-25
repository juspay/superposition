"""Context management tools for Superposition MCP server."""

from typing import Any, Dict

from mcp.types import TextContent

from ..schemas.context_schemas import (
    CreateContextSchema,
    GetContextSchema,
    UpdateContextSchema,
    DeleteContextSchema,
    ListContextsSchema,
    MoveContextSchema
)
from .base import BaseTool


class CreateContextTool(BaseTool):
    """Tool for creating new contexts."""
    
    @property
    def name(self) -> str:
        return "create_context"
    
    @property
    def description(self) -> str:
        return """Create a new context in Superposition for configuration overrides.
        
        Contexts define conditions under which specific configuration overrides should apply.
        They consist of a condition (dimension-value pairs) and the overrides to apply.
        
        Examples:
        - Simple context: {
            "context": {"user_type": "premium"},
            "override": {"feature_x": true},
            "change_reason": "Enable feature for premium users"
          }
        - Multi-dimensional: {
            "context": {"region": "us-east", "env": "prod"},
            "override": {"api_timeout": 5000, "cache_ttl": 300},
            "change_reason": "Optimize for US East production"
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
                "context": {
                    "type": "object",
                    "description": "Context condition as dimension-value pairs",
                    "additionalProperties": True
                },
                "override": {
                    "type": "object", 
                    "description": "Configuration overrides to apply for this context",
                    "additionalProperties": True
                },
                "description": {
                    "type": "string",
                    "description": "Human-readable description of this context"
                },
                "change_reason": {
                    "type": "string",
                    "description": "Reason for creating this context (required for audit)"
                },
                "config_tags": {
                    "type": "string",
                    "description": "Tags to associate with this context"
                }
            },
            "required": ["context", "override", "change_reason"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return CreateContextSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the create context operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.create_context(
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            context=validated_input.context,
            override=validated_input.override,
            description=validated_input.description,
            change_reason=validated_input.change_reason,
            config_tags=validated_input.config_tags
        )
        
        return self.format_success_response(result)


class GetContextTool(BaseTool):
    """Tool for retrieving a specific context by ID."""
    
    @property
    def name(self) -> str:
        return "get_context"
    
    @property
    def description(self) -> str:
        return """Get details of a specific context by its ID.
        
        This tool retrieves complete information about a context including its condition,
        overrides, metadata, and audit information.
        
        Examples:
        - Get by ID: {"context_id": "ctx_abc123"}
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
                "context_id": {
                    "type": "string",
                    "description": "Unique identifier of the context to retrieve"
                }
            },
            "required": ["context_id"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return GetContextSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the get context operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.get_context(
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            id=validated_input.context_id
        )
        
        return self.format_success_response(result)


class UpdateContextTool(BaseTool):
    """Tool for updating context overrides."""
    
    @property
    def name(self) -> str:
        return "update_context"
    
    @property
    def description(self) -> str:
        return """Update the overrides for an existing context.
        
        You can identify the context either by its ID or by its condition. The overrides
        will be completely replaced with the new values provided.
        
        Examples:
        - Update by ID: {
            "context_id": "ctx_123",
            "override": {"feature_y": false},
            "change_reason": "Disable feature Y"
          }
        - Update by condition: {
            "context": {"user_type": "premium"},
            "override": {"max_requests": 1000},
            "change_reason": "Increase rate limit"
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
                "context_id": {
                    "type": "string",
                    "description": "Context ID to update (use either this or context)"
                },
                "context": {
                    "type": "object",
                    "description": "Context condition to update (use either this or context_id)",
                    "additionalProperties": True
                },
                "override": {
                    "type": "object",
                    "description": "New configuration overrides",
                    "additionalProperties": True
                },
                "description": {
                    "type": "string",
                    "description": "Updated description for this context"
                },
                "change_reason": {
                    "type": "string",
                    "description": "Reason for updating this context (required for audit)"
                },
                "config_tags": {
                    "type": "string",
                    "description": "Tags to associate with this context update"
                }
            },
            "required": ["override", "change_reason"],
            "anyOf": [
                {"required": ["context_id"]},
                {"required": ["context"]}
            ],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return UpdateContextSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the update context operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.update_context(
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            context_id=validated_input.context_id,
            context=validated_input.context,
            override=validated_input.override,
            description=validated_input.description,
            change_reason=validated_input.change_reason,
            config_tags=validated_input.config_tags
        )
        
        return self.format_success_response(result)


class DeleteContextTool(BaseTool):
    """Tool for deleting contexts."""
    
    @property
    def name(self) -> str:
        return "delete_context"
    
    @property
    def description(self) -> str:
        return """Delete a context by its ID.
        
        This permanently removes the context and its overrides. Use with caution as this
        action cannot be undone.
        
        Examples:
        - Delete context: {"context_id": "ctx_abc123"}
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
                "context_id": {
                    "type": "string",
                    "description": "Unique identifier of the context to delete"
                },
                "config_tags": {
                    "type": "string",
                    "description": "Tags to associate with this deletion"
                }
            },
            "required": ["context_id"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return DeleteContextSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the delete context operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.delete_context(
            context_id=validated_input.context_id,
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            config_tags=validated_input.config_tags
        )
        
        return self.format_success_response(result)


class MoveContextTool(BaseTool):
    """Tool for moving contexts (changing their conditions)."""
    
    @property
    def name(self) -> str:
        return "move_context"
    
    @property
    def description(self) -> str:
        return """Move a context by changing its condition while keeping the same overrides.
        
        This is useful when you want to change when a context applies (its condition)
        without changing what it does (its overrides).
        
        Examples:
        - Move to different region: {
            "context_id": "ctx_123",
            "context": {"region": "eu-west", "user_type": "premium"},
            "change_reason": "Expand to EU region"
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
                "context_id": {
                    "type": "string",
                    "description": "Unique identifier of the context to move"
                },
                "context": {
                    "type": "object",
                    "description": "New context condition as dimension-value pairs",
                    "additionalProperties": True
                },
                "description": {
                    "type": "string",
                    "description": "Updated description for this context"
                },
                "change_reason": {
                    "type": "string",
                    "description": "Reason for moving this context (required for audit)"
                }
            },
            "required": ["context_id", "context", "change_reason"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return MoveContextSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the move context operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.move_context(
            context_id=validated_input.context_id,
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            context=validated_input.context,
            description=validated_input.description,
            change_reason=validated_input.change_reason
        )
        
        return self.format_success_response(result)


class ListContextsTool(BaseTool):
    """Tool for listing contexts with filtering and pagination."""
    
    @property
    def name(self) -> str:
        return "list_contexts"
    
    @property
    def description(self) -> str:
        return """List contexts with optional filtering and pagination.
        
        This tool allows you to retrieve contexts with various filtering options like
        prefix matching, creator filtering, and text search. Results are paginated
        and can be sorted by different criteria.
        
        Examples:
        - List all: {}
        - Paginated: {"page": 2, "count": 20}
        - Filter by prefix: {"prefix": "feature_"}
        - Search: {"plaintext": "premium user"}
        - Sort by creation: {"sort_on": "created_at", "sort_by": "DESC"}
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
                "page": {
                    "type": "integer",
                    "minimum": 1,
                    "description": "Page number for pagination"
                },
                "count": {
                    "type": "integer",
                    "minimum": 1,
                    "maximum": 1000,
                    "description": "Number of contexts to retrieve per page"
                },
                "prefix": {
                    "type": "string",
                    "description": "Filter contexts by configuration key prefix"
                },
                "sort_on": {
                    "type": "string",
                    "enum": ["last_modified_at", "created_at", "weight"],
                    "description": "Field to sort by"
                },
                "sort_by": {
                    "type": "string",
                    "enum": ["ASC", "DESC"],
                    "description": "Sort direction"
                },
                "created_by": {
                    "type": "string",
                    "description": "Filter by context creator"
                },
                "last_modified_by": {
                    "type": "string",
                    "description": "Filter by who last modified the context"
                },
                "plaintext": {
                    "type": "string",
                    "description": "Search term for plaintext search in contexts"
                }
            },
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return ListContextsSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the list contexts operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.list_contexts(
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            page=validated_input.page,
            count=validated_input.count,
            prefix=validated_input.prefix,
            sort_on=validated_input.sort_on,
            sort_by=validated_input.sort_by,
            created_by=validated_input.created_by,
            last_modified_by=validated_input.last_modified_by,
            plaintext=validated_input.plaintext
        )
        
        return self.format_success_response(result)