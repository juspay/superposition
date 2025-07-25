"""Configuration management for Superposition MCP Server."""

import os
from typing import Optional

from pydantic import BaseModel, Field


class SuperpositionConfig(BaseModel):
    """Configuration for Superposition API client."""
    
    # API endpoint configuration
    base_url: str = Field(
        default="http://localhost:8080",
        description="Base URL for Superposition API"
    )
    
    # Authentication
    bearer_token: Optional[str] = Field(
        default=None,
        description="Bearer token for API authentication"
    )
    
    # Default workspace and org
    default_workspace_id: str = Field(
        default="dev",
        description="Default workspace ID to use when not specified"
    )
    
    default_org_id: str = Field(
        default="juspay",
        description="Default organization ID to use when not specified"
    )
    
    # Request timeouts
    request_timeout: int = Field(
        default=30,
        description="Request timeout in seconds"
    )
    
    # Connection settings
    max_retries: int = Field(
        default=3,
        description="Maximum number of retries for failed requests"
    )
    
    retry_delay: float = Field(
        default=1.0,
        description="Base delay between retries in seconds"
    )

    @classmethod
    def from_env(cls) -> "SuperpositionConfig":
        """Create configuration from environment variables."""
        return cls(
            base_url=os.getenv("SUPERPOSITION_BASE_URL", "http://localhost:8080"),
            bearer_token=os.getenv("SUPERPOSITION_BEARER_TOKEN"),
            default_workspace_id=os.getenv("SUPERPOSITION_WORKSPACE_ID", "dev"),
            default_org_id=os.getenv("SUPERPOSITION_ORG_ID", "juspay"),
            request_timeout=int(os.getenv("SUPERPOSITION_REQUEST_TIMEOUT", "30")),
            max_retries=int(os.getenv("SUPERPOSITION_MAX_RETRIES", "3")),
            retry_delay=float(os.getenv("SUPERPOSITION_RETRY_DELAY", "1.0")),
        )


class MCPServerConfig(BaseModel):
    """Configuration for the MCP server itself."""
    
    # Server settings
    server_name: str = Field(
        default="superposition-mcp-server",
        description="Name of the MCP server"
    )
    
    server_version: str = Field(
        default="0.1.0",
        description="Version of the MCP server"
    )
    
    # Logging
    log_level: str = Field(
        default="INFO",
        description="Logging level (DEBUG, INFO, WARNING, ERROR)"
    )
    
    # Tool configuration
    enable_context_tools: bool = Field(
        default=True,
        description="Enable context-aware configuration tools"
    )
    
    enable_experiment_tools: bool = Field(
        default=True,
        description="Enable experimentation platform tools"
    )
    
    enable_workspace_tools: bool = Field(
        default=True,
        description="Enable workspace management tools"
    )

    @classmethod
    def from_env(cls) -> "MCPServerConfig":
        """Create configuration from environment variables."""
        return cls(
            server_name=os.getenv("MCP_SERVER_NAME", "superposition-mcp-server"),
            server_version=os.getenv("MCP_SERVER_VERSION", "0.1.0"),
            log_level=os.getenv("MCP_LOG_LEVEL", "INFO"),
            enable_context_tools=os.getenv("MCP_ENABLE_CONTEXT_TOOLS", "true").lower() == "true",
            enable_experiment_tools=os.getenv("MCP_ENABLE_EXPERIMENT_TOOLS", "true").lower() == "true",
            enable_workspace_tools=os.getenv("MCP_ENABLE_WORKSPACE_TOOLS", "true").lower() == "true",
        )