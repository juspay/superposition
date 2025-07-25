"""Experiment group management tools for Superposition MCP server."""

from typing import Any, Dict

from mcp.types import TextContent

from ..schemas.experiment_group_schemas import (
    CreateExperimentGroupSchema,
    GetExperimentGroupSchema,
    ListExperimentGroupsSchema,
    UpdateExperimentGroupSchema,
    DeleteExperimentGroupSchema,
    AddMembersToGroupSchema,
    RemoveMembersFromGroupSchema,
)
from .base import BaseTool


class CreateExperimentGroupTool(BaseTool):
    """Tool for creating an experiment group."""
    
    @property
    def name(self) -> str:
        return "create_experiment_group"
    
    @property
    def description(self) -> str:
        return """Create a new experiment group in Superposition.
        
        Experiment groups allow you to organize and manage related experiments together.
        
        Example:
        {
            "name": "mobile_ui_experiments",
            "description": "Group for all mobile UI related experiments",
            "created_by": "user@example.com"
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
                "name": {
                    "type": "string",
                    "description": "Name of the experiment group"
                },
                "description": {
                    "type": "string",
                    "description": "Description of the experiment group"
                },
                "created_by": {
                    "type": "string",
                    "description": "User who created the experiment group"
                }
            },
            "required": ["name", "created_by"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self) -> type[CreateExperimentGroupSchema]:
        return CreateExperimentGroupSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the create experiment group operation."""
        inputs = self.validate_input(arguments)
        
        result = await self.client.create_experiment_group(
            name=inputs.name,
            description=inputs.description,
            created_by=inputs.created_by,
            workspace_id=inputs.workspace_id,
            org_id=inputs.org_id
        )
        
        return self.format_success_response(result)


class GetExperimentGroupTool(BaseTool):
    """Tool for getting an experiment group."""
    
    @property
    def name(self) -> str:
        return "get_experiment_group"
    
    @property
    def description(self) -> str:
        return """Get an experiment group from Superposition.
        
        Retrieves the details of a specific experiment group including
        its metadata and member experiments.
        
        Example:
        {
            "group_id": "group_123"
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
                "group_id": {
                    "type": "string",
                    "description": "Experiment group ID to retrieve"
                }
            },
            "required": ["group_id"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self) -> type[GetExperimentGroupSchema]:
        return GetExperimentGroupSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the get experiment group operation."""
        inputs = self.validate_input(arguments)
        
        result = await self.client.get_experiment_group(
            group_id=inputs.group_id,
            workspace_id=inputs.workspace_id,
            org_id=inputs.org_id
        )
        
        return self.format_success_response(result)


class ListExperimentGroupsTool(BaseTool):
    """Tool for listing experiment groups."""
    
    @property
    def name(self) -> str:
        return "list_experiment_groups"
    
    @property
    def description(self) -> str:
        return """List experiment groups in Superposition.
        
        Retrieves a paginated list of experiment groups with optional filtering.
        
        Example:
        {
            "count": 20,
            "page": 1
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
                "count": {
                    "type": "integer",
                    "minimum": 1,
                    "maximum": 1000,
                    "default": 50,
                    "description": "Number of experiment groups to retrieve per page"
                },
                "page": {
                    "type": "integer",
                    "minimum": 1,
                    "default": 1,
                    "description": "Page number for pagination"
                }
            },
            "additionalProperties": False
        }
    
    @property
    def schema_class(self) -> type[ListExperimentGroupsSchema]:
        return ListExperimentGroupsSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the list experiment groups operation."""
        inputs = self.validate_input(arguments)
        
        result = await self.client.list_experiment_groups(
            count=inputs.count,
            page=inputs.page,
            workspace_id=inputs.workspace_id,
            org_id=inputs.org_id
        )
        
        return self.format_success_response(result)


class UpdateExperimentGroupTool(BaseTool):
    """Tool for updating an experiment group."""
    
    @property
    def name(self) -> str:
        return "update_experiment_group"
    
    @property
    def description(self) -> str:
        return """Update an existing experiment group in Superposition.
        
        Updates the name or description of an existing experiment group.
        Only provided fields will be updated.
        
        Example:
        {
            "group_id": "group_123",
            "name": "updated_mobile_ui_experiments",
            "description": "Updated description for mobile UI experiments"
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
                "group_id": {
                    "type": "string",
                    "description": "Experiment group ID to update"
                },
                "name": {
                    "type": "string",
                    "description": "Updated name of the experiment group"
                },
                "description": {
                    "type": "string",
                    "description": "Updated description of the experiment group"
                }
            },
            "required": ["group_id"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self) -> type[UpdateExperimentGroupSchema]:
        return UpdateExperimentGroupSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the update experiment group operation."""
        inputs = self.validate_input(arguments)
        
        result = await self.client.update_experiment_group(
            group_id=inputs.group_id,
            name=inputs.name,
            description=inputs.description,
            workspace_id=inputs.workspace_id,
            org_id=inputs.org_id
        )
        
        return self.format_success_response(result)


class DeleteExperimentGroupTool(BaseTool):
    """Tool for deleting an experiment group."""
    
    @property
    def name(self) -> str:
        return "delete_experiment_group"
    
    @property
    def description(self) -> str:
        return """Delete an experiment group from Superposition.
        
        Permanently removes an experiment group. Note that this does not
        delete the experiments themselves, only the group organization.
        
        Example:
        {
            "group_id": "group_123"
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
                "group_id": {
                    "type": "string",
                    "description": "Experiment group ID to delete"
                }
            },
            "required": ["group_id"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self) -> type[DeleteExperimentGroupSchema]:
        return DeleteExperimentGroupSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the delete experiment group operation."""
        inputs = self.validate_input(arguments)
        
        result = await self.client.delete_experiment_group(
            group_id=inputs.group_id,
            workspace_id=inputs.workspace_id,
            org_id=inputs.org_id
        )
        
        return self.format_success_response(result)


class AddMembersToGroupTool(BaseTool):
    """Tool for adding members to an experiment group."""
    
    @property
    def name(self) -> str:
        return "add_members_to_group"
    
    @property
    def description(self) -> str:
        return """Add experiment members to an experiment group.
        
        Adds one or more experiments to an existing experiment group.
        
        Example:
        {
            "group_id": "group_123",
            "member_ids": ["exp_456", "exp_789"]
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
                "group_id": {
                    "type": "string",
                    "description": "Experiment group ID to add members to"
                },
                "member_ids": {
                    "type": "array",
                    "items": {"type": "string"},
                    "description": "List of experiment IDs to add to the group"
                }
            },
            "required": ["group_id", "member_ids"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self) -> type[AddMembersToGroupSchema]:
        return AddMembersToGroupSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the add members to group operation."""
        inputs = self.validate_input(arguments)
        
        result = await self.client.add_members_to_group(
            group_id=inputs.group_id,
            member_ids=inputs.member_ids,
            workspace_id=inputs.workspace_id,
            org_id=inputs.org_id
        )
        
        return self.format_success_response(result)


class RemoveMembersFromGroupTool(BaseTool):
    """Tool for removing members from an experiment group."""
    
    @property
    def name(self) -> str:
        return "remove_members_from_group"
    
    @property
    def description(self) -> str:
        return """Remove experiment members from an experiment group.
        
        Removes one or more experiments from an existing experiment group.
        
        Example:
        {
            "group_id": "group_123",
            "member_ids": ["exp_456", "exp_789"]
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
                "group_id": {
                    "type": "string",
                    "description": "Experiment group ID to remove members from"
                },
                "member_ids": {
                    "type": "array",
                    "items": {"type": "string"},
                    "description": "List of experiment IDs to remove from the group"
                }
            },
            "required": ["group_id", "member_ids"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self) -> type[RemoveMembersFromGroupSchema]:
        return RemoveMembersFromGroupSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the remove members from group operation."""
        inputs = self.validate_input(arguments)
        
        result = await self.client.remove_members_from_group(
            group_id=inputs.group_id,
            member_ids=inputs.member_ids,
            workspace_id=inputs.workspace_id,
            org_id=inputs.org_id
        )
        
        return self.format_success_response(result)