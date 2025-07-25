"""Configuration management tools for Superposition MCP server."""

from typing import Any, Dict

from mcp.types import TextContent

from ..schemas.config_schemas import (
    GetConfigSchema,
    GetResolvedConfigSchema, 
    ListConfigVersionsSchema
)
from .base import BaseTool


class GetConfigTool(BaseTool):
    """Tool for retrieving configuration with context."""
    
    @property
    def name(self) -> str:
        return "get_config"
    
    @property
    def description(self) -> str:
        return """Get configuration values from Superposition with optional context filtering.
        
        This tool retrieves configuration values, optionally filtered by context conditions.
        Context is a map of dimension-value pairs that determine which configuration overrides apply.
        
        Examples:
        - Get all config: {}
        - Get with context: {"context": {"user_type": "premium", "region": "us-east"}}
        - Get specific version: {"version": "v1.2.3"}
        - Get with prefix: {"prefix": "feature_flags"}
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
                "prefix": {
                    "type": "string",
                    "description": "Configuration key prefix to filter results"
                },
                "version": {
                    "type": "string",
                    "description": "Specific configuration version to retrieve"
                },
                "context": {
                    "type": "object",
                    "description": "Context map for configuration resolution (dimension -> value pairs)",
                    "additionalProperties": True
                }
            },
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return GetConfigSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the get configuration operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.get_config(
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            prefix=validated_input.prefix,
            version=validated_input.version,
            context=validated_input.context
        )
        
        return self.format_success_response(result)


class GetResolvedConfigTool(BaseTool):
    """Tool for retrieving resolved configuration."""
    
    @property
    def name(self) -> str:
        return "get_resolved_config"
    
    @property
    def description(self) -> str:
        return """Get fully resolved configuration from Superposition.
        
        This tool returns the final resolved configuration after applying all context-based overrides.
        It's useful for getting the exact configuration values that would be used in a specific context.
        
        Examples:
        - Basic resolution: {"context": {"user_type": "premium"}}
        - With reasoning: {"context": {"region": "us"}, "show_reasoning": true}
        - Specific context ID: {"context_id": "ctx_123"}
        - Replace strategy: {"context": {"env": "prod"}, "merge_strategy": "REPLACE"}
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
                "prefix": {
                    "type": "string", 
                    "description": "Configuration key prefix to filter results"
                },
                "version": {
                    "type": "string",
                    "description": "Specific configuration version to retrieve"
                },
                "context": {
                    "type": "object",
                    "description": "Context map for configuration resolution (dimension -> value pairs)",
                    "additionalProperties": True
                },
                "context_id": {
                    "type": "string",
                    "description": "Specific context ID to use for resolution"
                },
                "show_reasoning": {
                    "type": "boolean",
                    "description": "Include reasoning information in the response"
                },
                "merge_strategy": {
                    "type": "string",
                    "enum": ["MERGE", "REPLACE"],
                    "description": "Strategy for merging configurations"
                }
            },
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return GetResolvedConfigSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the get resolved configuration operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.get_resolved_config(
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            prefix=validated_input.prefix,
            version=validated_input.version,
            context=validated_input.context,
            context_id=validated_input.context_id,
            show_reasoning=validated_input.show_reasoning,
            merge_strategy=validated_input.merge_strategy
        )
        
        return self.format_success_response(result)


class ListConfigVersionsTool(BaseTool):
    """Tool for listing configuration versions."""
    
    @property
    def name(self) -> str:
        return "list_config_versions"
    
    @property
    def description(self) -> str:
        return """List available configuration versions in Superposition.
        
        This tool retrieves a paginated list of configuration versions, showing the history
        of configuration changes over time. Each version includes metadata like creation time,
        description, and tags.
        
        Examples:
        - List recent versions: {}
        - Paginated: {"page": 2, "count": 10}
        - Large page: {"count": 100}
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
                    "description": "Number of versions to retrieve per page (default: 50)"
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
        return ListConfigVersionsSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the list configuration versions operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.list_config_versions(
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            count=validated_input.count,
            page=validated_input.page
        )
        
        return self.format_success_response(result)