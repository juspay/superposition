"""Default configuration management tools for Superposition MCP server."""

from typing import Any, Dict

from mcp.types import TextContent

from ..schemas.default_config_schemas import (
    CreateDefaultConfigSchema,
    ListDefaultConfigsSchema,
    UpdateDefaultConfigSchema,
    DeleteDefaultConfigSchema
)
from .base import BaseTool


class CreateDefaultConfigTool(BaseTool):
    """Tool for creating new default configurations."""
    
    @property
    def name(self) -> str:
        return "create_default_config"
    
    @property
    def description(self) -> str:
        return """Create a new default configuration value in Superposition.
        
        Default configurations provide the base values for configuration keys before
        any context-based overrides are applied. They serve as fallbacks when no
        specific context matches.
        
        Examples:
        - Basic config: {"key": "api_timeout", "value": 30, "created_by": "admin"}
        - With schema: {
            "key": "feature_flags", 
            "value": {"new_ui": false}, 
            "schema": {"type": "object"},
            "description": "Feature flag toggles",
            "created_by": "admin"
          }
        - String config: {"key": "app_name", "value": "MyApp", "created_by": "admin"}
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
                "key": {
                    "type": "string",
                    "description": "Configuration key"
                },
                "value": {
                    "description": "Configuration value (can be any JSON-serializable type)"
                },
                "schema": {
                    "type": "object",
                    "description": "JSON schema for validating the configuration value",
                    "additionalProperties": True
                },
                "description": {
                    "type": "string",
                    "description": "Description of what this configuration key controls"
                },
                "created_by": {
                    "type": "string",
                    "description": "User who created this configuration"
                }
            },
            "required": ["key", "value", "created_by"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return CreateDefaultConfigSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the create default config operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.create_default_config(
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            key=validated_input.key,
            value=validated_input.value,
            schema=validated_input.json_schema,
            description=validated_input.description,
            created_by=validated_input.created_by
        )
        
        return self.format_success_response(result)


class ListDefaultConfigsTool(BaseTool):
    """Tool for listing default configurations."""
    
    @property
    def name(self) -> str:
        return "list_default_configs"
    
    @property
    def description(self) -> str:
        return """List default configurations with pagination and filtering support.
        
        Returns a paginated list of default configuration key-value pairs.
        Useful for understanding the base configuration state before any context
        overrides are applied.
        
        Examples:
        - List all configs: {}
        - Paginated: {"page": 2, "count": 20}
        - Filter by prefix: {"prefix": "feature_"}
        - Combined: {"prefix": "api_", "count": 10}
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
                    "description": "Number of configurations to retrieve per page (default: 50)"
                },
                "page": {
                    "type": "integer",
                    "minimum": 1,
                    "description": "Page number for pagination (default: 1)"
                },
                "prefix": {
                    "type": "string",
                    "description": "Filter configurations by key prefix"
                }
            },
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return ListDefaultConfigsSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the list default configs operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.list_default_configs(
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            count=validated_input.count,
            page=validated_input.page,
            prefix=validated_input.prefix
        )
        
        return self.format_success_response(result)


class UpdateDefaultConfigTool(BaseTool):
    """Tool for updating default configurations."""
    
    @property
    def name(self) -> str:
        return "update_default_config"
    
    @property
    def description(self) -> str:
        return """Update an existing default configuration value.
        
        Updates the value, schema, or description of an existing default configuration.
        This change will affect the fallback value used when no context-specific
        overrides match.
        
        Examples:
        - Update value: {"key": "api_timeout", "value": 45}
        - Update with schema: {"key": "feature_flags", "value": {"new_ui": true}, "schema": {"type": "object"}}
        - Update description: {"key": "app_name", "value": "MyApp v2", "description": "Application display name"}
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
                "key": {
                    "type": "string",
                    "description": "Configuration key to update"
                },
                "value": {
                    "description": "New configuration value (can be any JSON-serializable type)"
                },
                "schema": {
                    "type": "object",
                    "description": "Updated JSON schema for validating the configuration value",
                    "additionalProperties": True
                },
                "description": {
                    "type": "string",
                    "description": "Updated description of what this configuration key controls"
                }
            },
            "required": ["key", "value"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return UpdateDefaultConfigSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the update default config operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.update_default_config(
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            key=validated_input.key,
            value=validated_input.value,
            schema=validated_input.json_schema,
            description=validated_input.description
        )
        
        return self.format_success_response(result)


class DeleteDefaultConfigTool(BaseTool):
    """Tool for deleting default configurations."""
    
    @property
    def name(self) -> str:
        return "delete_default_config"
    
    @property
    def description(self) -> str:
        return """Delete a default configuration key-value pair.
        
        ⚠️  WARNING: Deleting a default configuration will remove the fallback value
        for this key. If no context-specific overrides exist, the key will be
        undefined in resolved configurations.
        
        Examples:
        - Delete config: {"config_key": "old_feature_flag"}
        - Delete from specific workspace: {"config_key": "temp_setting", "workspace_id": "test"}
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
                "config_key": {
                    "type": "string",
                    "description": "Configuration key to delete"
                }
            },
            "required": ["config_key"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return DeleteDefaultConfigSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the delete default config operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.delete_default_config(
            config_key=validated_input.config_key,
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id
        )
        
        return self.format_success_response(result)