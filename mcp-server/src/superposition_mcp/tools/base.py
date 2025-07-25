"""Base tool class for Superposition MCP tools."""

import json
import logging
from abc import ABC, abstractmethod
from typing import Any, Dict, List, Optional

from mcp.types import Tool, TextContent
from pydantic import BaseModel, ValidationError

from ..client import SuperpositionClient, SuperpositionClientError


logger = logging.getLogger(__name__)


class BaseTool(ABC):
    """Base class for all Superposition MCP tools."""
    
    def __init__(self, client: SuperpositionClient):
        """Initialize the tool with a Superposition client.
        
        Args:
            client: The Superposition client to use for API calls
        """
        self.client = client
        self.logger = logging.getLogger(f"{__name__}.{self.__class__.__name__}")
    
    @property
    @abstractmethod
    def name(self) -> str:
        """The name of the tool."""
        pass
    
    @property
    @abstractmethod
    def description(self) -> str:
        """Description of what the tool does."""
        pass
    
    @property
    @abstractmethod
    def input_schema(self) -> Dict[str, Any]:
        """JSON schema for the tool's input parameters."""
        pass
    
    @property
    @abstractmethod
    def schema_class(self) -> type[BaseModel]:
        """Pydantic schema class for input validation."""
        pass
    
    def get_tool_definition(self) -> Tool:
        """Get the MCP tool definition."""
        return Tool(
            name=self.name,
            description=self.description,
            inputSchema=self.input_schema
        )
    
    def validate_input(self, arguments: Dict[str, Any]) -> BaseModel:
        """Validate and parse input arguments using the schema class.
        
        Args:
            arguments: Raw input arguments from MCP call
            
        Returns:
            Validated input object
            
        Raises:
            ValidationError: If input validation fails
        """
        try:
            # Log the input for debugging (redact sensitive fields)
            safe_args = self._redact_sensitive_fields(arguments.copy())
            self.logger.debug(f"Validating input for {self.name}: {safe_args}")
            
            return self.schema_class(**arguments)
        except ValidationError as e:
            self.logger.error(f"Input validation failed for {self.name}: {e}")
            # Add more helpful error message
            detailed_errors = []
            for error in e.errors():
                field = ".".join(str(x) for x in error["loc"])
                message = error["msg"]
                detailed_errors.append(f"Field '{field}': {message}")
            
            raise ValidationError(f"Validation failed for {self.name}. Errors: {'; '.join(detailed_errors)}")
    
    def _redact_sensitive_fields(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Redact sensitive fields from input data for logging."""
        sensitive_fields = ['bearer_token', 'token', 'password', 'secret', 'key']
        for field in sensitive_fields:
            if field in data:
                data[field] = "[REDACTED]"
        return data
    
    def _validate_required_fields(self, data: Dict[str, Any], required_fields: List[str]) -> None:
        """Validate that required fields are present and non-empty.
        
        Args:
            data: Input data dictionary
            required_fields: List of field names that are required
            
        Raises:
            ValidationError: If any required field is missing or empty
        """
        missing_fields = []
        empty_fields = []
        
        for field in required_fields:
            if field not in data:
                missing_fields.append(field)
            elif data[field] is None or (isinstance(data[field], str) and not data[field].strip()):
                empty_fields.append(field)
        
        if missing_fields or empty_fields:
            error_msg = []
            if missing_fields:
                error_msg.append(f"Missing required fields: {', '.join(missing_fields)}")
            if empty_fields:
                error_msg.append(f"Empty required fields: {', '.join(empty_fields)}")
            raise ValidationError("; ".join(error_msg))
    
    def format_success_response(self, result: Any) -> list[TextContent]:
        """Format a successful API response for MCP.
        
        Args:
            result: The result from the Superposition API
            
        Returns:
            List of MCP TextContent objects
        """
        try:
            # Convert result to a serializable format
            if hasattr(result, '__dict__'):
                # Handle dataclass objects from SDK
                result_dict = self._dataclass_to_dict(result)
            else:
                result_dict = result
            
            formatted_result = json.dumps(result_dict, indent=2, default=str)
            
            return [
                TextContent(
                    type="text",
                    text=f"✅ **{self.name} - Success**\n\n```json\n{formatted_result}\n```"
                )
            ]
        except Exception as e:
            self.logger.error(f"Failed to format success response: {e}")
            return [
                TextContent(
                    type="text", 
                    text=f"✅ **{self.name} - Success**\n\nOperation completed successfully, but failed to format response: {e}"
                )
            ]
    
    def format_error_response(self, error: Exception) -> list[TextContent]:
        """Format an error response for MCP.
        
        Args:
            error: The exception that occurred
            
        Returns:
            List of MCP TextContent objects
        """
        error_type = type(error).__name__
        error_message = str(error)
        
        # Provide specific guidance based on error type
        guidance = ""
        troubleshooting = ""
        
        if isinstance(error, ValidationError):
            guidance = "\\n\\n**Issue:** Input validation failed"
            troubleshooting = "\\n\\n**Fix:** Check the input parameters and ensure they match the expected schema. Common issues include missing required fields, wrong data types, or invalid enum values."
            
        elif isinstance(error, SuperpositionClientError):
            if isinstance(error, SuperpositionAuthError):
                guidance = "\\n\\n**Issue:** Authentication or authorization failed"
                troubleshooting = "\\n\\n**Fix:** Ensure your bearer token is valid and you have the necessary permissions for this operation."
            
            elif isinstance(error, SuperpositionNotFoundError):
                guidance = "\\n\\n**Issue:** Resource not found"
                troubleshooting = "\\n\\n**Fix:** Verify the resource ID exists and you have access to it. Check for typos in the ID."
            
            elif isinstance(error, SuperpositionValidationError):
                guidance = "\\n\\n**Issue:** Request validation failed"
                troubleshooting = "\\n\\n**Fix:** Check the input data format, required fields, and ensure values are within acceptable ranges."
            
            elif "connection" in error_message.lower() or "timeout" in error_message.lower():
                guidance = "\\n\\n**Issue:** Network connectivity problem"
                troubleshooting = "\\n\\n**Fix:** Check your network connection and verify the Superposition server is accessible."
            
            elif "rate limit" in error_message.lower():
                guidance = "\\n\\n**Issue:** API rate limit exceeded"
                troubleshooting = "\\n\\n**Fix:** Wait a moment before retrying. Consider reducing the frequency of API calls."
            
            else:
                guidance = "\\n\\n**Issue:** API operation failed"
                troubleshooting = "\\n\\n**Fix:** Check the server logs for more details. Verify your request parameters are correct."
        
        else:
            guidance = "\\n\\n**Issue:** Unexpected error occurred"
            troubleshooting = "\\n\\n**Fix:** This may be a system error. Please check the server logs for more details."
        
        return [
            TextContent(
                type="text",
                text=f"❌ **{self.name} - Error**\\n\\n**{error_type}:** {error_message}{guidance}{troubleshooting}"
            )
        ]
    
    def _dataclass_to_dict(self, obj: Any) -> Any:
        """Convert a dataclass object to a dictionary recursively."""
        if hasattr(obj, '__dataclass_fields__'):
            # It's a dataclass
            result = {}
            for field_name in obj.__dataclass_fields__:
                value = getattr(obj, field_name)
                result[field_name] = self._dataclass_to_dict(value)
            return result
        elif isinstance(obj, list):
            return [self._dataclass_to_dict(item) for item in obj]
        elif isinstance(obj, dict):
            return {key: self._dataclass_to_dict(value) for key, value in obj.items()}
        else:
            return obj
    
    @abstractmethod
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the tool with the given arguments.
        
        Args:
            arguments: Input arguments for the tool
            
        Returns:
            List of MCP TextContent objects with the response
        """
        pass
    
    async def safe_execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Safely execute the tool with comprehensive error handling.
        
        Args:
            arguments: Input arguments for the tool
            
        Returns:
            List of MCP TextContent objects with the response
        """
        try:
            self.logger.info(f"Executing {self.name} with arguments: {arguments}")
            result = await self.execute(arguments)
            self.logger.info(f"Successfully executed {self.name}")
            return result
            
        except ValidationError as e:
            self.logger.error(f"Validation error in {self.name}: {e}")
            return self.format_error_response(e)
            
        except SuperpositionClientError as e:
            self.logger.error(f"Client error in {self.name}: {e}")
            return self.format_error_response(e)
            
        except Exception as e:
            self.logger.error(f"Unexpected error in {self.name}: {e}")
            return self.format_error_response(e)