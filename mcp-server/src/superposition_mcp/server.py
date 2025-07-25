"""Main MCP server implementation for Superposition."""

import asyncio
import logging
from typing import Any, Dict, List, Optional

from mcp import ClientSession
from mcp.server import Server
from mcp.server.stdio import stdio_server
from mcp.types import (
    CallToolRequest,
    ListToolsRequest,
    Tool,
    TextContent,
)

from .client import SuperpositionClient
from .config import SuperpositionConfig, MCPServerConfig
from .tools import (
    # Config tools
    GetConfigTool,
    GetResolvedConfigTool,
    ListConfigVersionsTool,
    # Context tools
    CreateContextTool,
    GetContextTool,
    UpdateContextTool,
    DeleteContextTool,
    ListContextsTool,
    MoveContextTool,
    # Experiment tools
    CreateExperimentTool,
    GetExperimentTool,
    UpdateExperimentTool,
    ConcludeExperimentTool,
    DiscardExperimentTool,
    RampExperimentTool,
    PauseExperimentTool,
    ResumeExperimentTool,
    ListExperimentsTool,
    ApplicableVariantsTool,
    # Dimension tools
    CreateDimensionTool,
    GetDimensionTool,
    ListDimensionsTool,
    UpdateDimensionTool,
    DeleteDimensionTool,
    # Default config tools
    CreateDefaultConfigTool,
    ListDefaultConfigsTool,
    UpdateDefaultConfigTool,
    DeleteDefaultConfigTool,
    # Function tools
    CreateFunctionTool,
    GetFunctionTool,
    ListFunctionTool,
    UpdateFunctionTool,
    DeleteFunctionTool,
    # Type template tools
    CreateTypeTemplateTool,
    GetTypeTemplateListTool,
    UpdateTypeTemplateTool,
    DeleteTypeTemplateTool,
    # Experiment group tools
    CreateExperimentGroupTool,
    GetExperimentGroupTool,
    ListExperimentGroupsTool,
    UpdateExperimentGroupTool,
    DeleteExperimentGroupTool,
    AddMembersToGroupTool,
    RemoveMembersFromGroupTool,
)


logger = logging.getLogger(__name__)


class SuperpositionMCPServer:
    """Main Superposition MCP Server implementation."""

    def __init__(
        self,
        superposition_config: Optional[SuperpositionConfig] = None,
        mcp_config: Optional[MCPServerConfig] = None
    ):
        """Initialize the MCP server.

        Args:
            superposition_config: Configuration for Superposition API client
            mcp_config: Configuration for the MCP server itself
        """
        self.superposition_config = superposition_config or SuperpositionConfig.from_env()
        self.mcp_config = mcp_config or MCPServerConfig.from_env()

        # Set up logging
        self._setup_logging()

        # Initialize the Superposition client
        self.client = SuperpositionClient(self.superposition_config)

        # Initialize the MCP server
        self.server = Server(self.mcp_config.server_name)

        # Initialize tools
        self.tools = {}
        self._register_tools()

        # Set up server handlers
        self._setup_handlers()

    def _setup_logging(self):
        """Set up logging configuration."""
        logging.basicConfig(
            level=getattr(logging, self.mcp_config.log_level.upper()),
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
        )
        self.logger = logging.getLogger(f"{__name__}.{self.__class__.__name__}")
        self.logger.info(f"Initializing Superposition MCP Server v{self.mcp_config.server_version}")

    def _register_tools(self):
        """Register all available tools based on configuration."""
        tool_classes = []

        # Configuration tools
        if self.mcp_config.enable_context_tools:
            tool_classes.extend([
                GetConfigTool,
                GetResolvedConfigTool,
                ListConfigVersionsTool,
            ])

        # Context management tools
        if self.mcp_config.enable_context_tools:
            tool_classes.extend([
                CreateContextTool,
                GetContextTool,
                UpdateContextTool,
                DeleteContextTool,
                ListContextsTool,
                MoveContextTool,
            ])

        # Dimension management tools
        if self.mcp_config.enable_context_tools:
            tool_classes.extend([
                CreateDimensionTool,
                GetDimensionTool,
                ListDimensionsTool,
                UpdateDimensionTool,
                DeleteDimensionTool,
            ])

        # Default configuration tools
        if self.mcp_config.enable_context_tools:
            tool_classes.extend([
                CreateDefaultConfigTool,
                ListDefaultConfigsTool,
                UpdateDefaultConfigTool,
                DeleteDefaultConfigTool,
            ])

        # Experiment tools
        if self.mcp_config.enable_experiment_tools:
            tool_classes.extend([
                CreateExperimentTool,
                GetExperimentTool,
                UpdateExperimentTool,
                ConcludeExperimentTool,
                DiscardExperimentTool,
                RampExperimentTool,
                PauseExperimentTool,
                ResumeExperimentTool,
                ListExperimentsTool,
                ApplicableVariantsTool,
                # Experiment group tools
                CreateExperimentGroupTool,
                GetExperimentGroupTool,
                ListExperimentGroupsTool,
                UpdateExperimentGroupTool,
                DeleteExperimentGroupTool,
                AddMembersToGroupTool,
                RemoveMembersFromGroupTool,
            ])

        # Function tools
        if self.mcp_config.enable_context_tools:  # Functions are part of context management
            tool_classes.extend([
                CreateFunctionTool,
                GetFunctionTool,
                ListFunctionTool,
                UpdateFunctionTool,
                DeleteFunctionTool,
            ])

        # Type template tools
        if self.mcp_config.enable_context_tools:  # Type templates are part of context management
            tool_classes.extend([
                CreateTypeTemplateTool,
                GetTypeTemplateListTool,
                UpdateTypeTemplateTool,
                DeleteTypeTemplateTool,
            ])

        # Create tool instances
        for tool_class in tool_classes:
            tool = tool_class(self.client)
            self.tools[tool.name] = tool
            self.logger.debug(f"Registered tool: {tool.name}")

        self.logger.info(f"Registered {len(self.tools)} tools")

    def _setup_handlers(self):
        """Set up MCP server request handlers."""

        @self.server.list_tools()
        async def list_tools(request: ListToolsRequest) -> List[Tool]:
            """Handle list tools request."""
            self.logger.debug("Received list_tools request")

            tools = []
            for tool in self.tools.values():
                tools.append(tool.get_tool_definition())

            self.logger.debug(f"Returning {len(tools)} tools")
            return tools

        @self.server.call_tool()
        async def call_tool(request: CallToolRequest) -> List[TextContent]:
            """Handle tool execution request."""
            tool_name = request.params.name
            arguments = request.params.arguments or {}

            self.logger.info(f"Executing tool: {tool_name}")
            self.logger.debug(f"Tool arguments: {arguments}")

            # Check if tool exists
            if tool_name not in self.tools:
                error_msg = f"Unknown tool: {tool_name}"
                self.logger.error(error_msg)
                return [
                    TextContent(
                        type="text",
                        text=f"❌ **Error**: {error_msg}\\n\\nAvailable tools: {', '.join(self.tools.keys())}"
                    )
                ]

            # Execute the tool
            tool = self.tools[tool_name]
            try:
                result = await tool.safe_execute(arguments)
                self.logger.info(f"Successfully executed tool: {tool_name}")
                return result
            except Exception as e:
                self.logger.error(f"Unexpected error executing tool {tool_name}: {e}")
                return [
                    TextContent(
                        type="text",
                        text=f"❌ **Unexpected Error**: {e}\\n\\nPlease check the server logs for more details."
                    )
                ]

    async def run_stdio(self):
        """Run the server using stdio transport."""
        self.logger.info("Starting Superposition MCP Server with stdio transport")

        try:
            async with stdio_server() as (read_stream, write_stream):
                self.logger.info("Server started successfully")
                await self.server.run(
                    read_stream,
                    write_stream,
                    self.server.create_initialization_options()
                )
        except BaseExceptionGroup as eg:
            # Handle ExceptionGroup (TaskGroup errors)
            self.logger.error(f"ExceptionGroup caught with {len(eg.exceptions)} exceptions")
            for i, exc in enumerate(eg.exceptions):
                self.logger.error(f"Exception {i}: {type(exc).__name__}: {exc}")
                import traceback
                self.logger.error(f"Traceback {i}: {''.join(traceback.format_exception(type(exc), exc, exc.__traceback__))}")
            raise
        except Exception as e:
            self.logger.error(f"Server error: {e}")
            import traceback
            self.logger.error(f"Traceback: {traceback.format_exc()}")
            raise
        finally:
            await self.cleanup()

    async def cleanup(self):
        """Clean up resources."""
        self.logger.info("Cleaning up server resources")
        try:
            await self.client.close()
        except Exception as e:
            self.logger.warning(f"Error during cleanup: {e}")


async def main():
    """Main entry point for the MCP server."""
    # Create and run the server
    server = SuperpositionMCPServer()

    try:
        await server.run_stdio()
    except KeyboardInterrupt:
        logger.info("Server interrupted by user")
    except BaseExceptionGroup as eg:
        # Handle ExceptionGroup (TaskGroup errors)
        logger.error(f"ExceptionGroup in main with {len(eg.exceptions)} exceptions")
        for i, exc in enumerate(eg.exceptions):
            logger.error(f"Exception {i}: {type(exc).__name__}: {exc}")
            import traceback
            logger.error(f"Traceback {i}: {''.join(traceback.format_exception(type(exc), exc, exc.__traceback__))}")
        raise
    except Exception as e:
        logger.error(f"Server failed: {e}")
        import traceback
        logger.error(f"Traceback: {traceback.format_exc()}")
        raise


def cli_main():
    """CLI entry point for the MCP server."""
    try:
        asyncio.run(main(), debug = True)
    except KeyboardInterrupt:
        print("\\nServer stopped by user")
    except Exception as e:
        print(f"Server failed: {e}")
        exit(1)


if __name__ == "__main__":
    cli_main()
