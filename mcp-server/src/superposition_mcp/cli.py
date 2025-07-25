#!/usr/bin/env python3
"""Command-line interface for Superposition MCP Server."""

import asyncio
import sys
import logging
from pathlib import Path

import click
from mcp.server.stdio import stdio_server


@click.command()
@click.option('--log-level', default='INFO', 
              type=click.Choice(['DEBUG', 'INFO', 'WARNING', 'ERROR'], case_sensitive=False),
              help='Set the logging level')
@click.option('--config-file', type=click.Path(exists=True),
              help='Path to configuration file')
def main(log_level: str, config_file: str):
    """Start the Superposition MCP Server."""
    
    # Configure logging
    logging.basicConfig(
        level=getattr(logging, log_level.upper()),
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    logger = logging.getLogger(__name__)
    logger.info(f"Starting Superposition MCP Server with log level: {log_level}")
    
    try:
        from .main import main as run_main
        
        # Run the main server implementation
        asyncio.run(run_main())
        
    except KeyboardInterrupt:
        logger.info("Server interrupted by user")
        sys.exit(0)
    except Exception as e:
        logger.error(f"Server failed: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()