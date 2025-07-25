"""Experiment management tools for Superposition MCP server."""

from typing import Any, Dict

from mcp.types import TextContent

from ..schemas.experiment_schemas import (
    CreateExperimentSchema,
    GetExperimentSchema,
    UpdateExperimentSchema,
    ConcludeExperimentSchema,
    DiscardExperimentSchema,
    RampExperimentSchema,
    PauseExperimentSchema,
    ResumeExperimentSchema,
    ListExperimentsSchema,
    ApplicableVariantsSchema
)
from .base import BaseTool


class CreateExperimentTool(BaseTool):
    """Tool for creating new experiments."""
    
    @property
    def name(self) -> str:
        return "create_experiment"
    
    @property
    def description(self) -> str:
        return """Create a new A/B experiment in Superposition.
        
        Experiments allow you to test different configuration values with different user cohorts.
        Each experiment defines a context (when it applies) and variants (what configurations to test).
        
        Examples:
        - Simple A/B test: {
            "name": "Button Color Test",
            "context": {"page": "checkout"},
            "variants": [
              {"id": "control", "variant_type": "CONTROL", "overrides": {"button_color": "blue"}},
              {"id": "variant_a", "variant_type": "EXPERIMENTAL", "overrides": {"button_color": "green"}}
            ],
            "description": "Test green vs blue button on checkout",
            "change_reason": "Optimize conversion rate"
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
                    "description": "Name of the experiment"
                },
                "experiment_type": {
                    "type": "string",
                    "enum": ["DEFAULT", "DELETE_OVERRIDES"],
                    "description": "Type of experiment"
                },
                "context": {
                    "type": "object",
                    "description": "Context condition for the experiment",
                    "additionalProperties": True
                },
                "variants": {
                    "type": "array",
                    "description": "List of experiment variants",
                    "items": {
                        "type": "object",
                        "properties": {
                            "id": {"type": "string", "description": "Unique variant ID"},
                            "variant_type": {
                                "type": "string", 
                                "enum": ["CONTROL", "EXPERIMENTAL"],
                                "description": "Type of variant"
                            },
                            "overrides": {
                                "type": "object",
                                "description": "Configuration overrides for this variant",
                                "additionalProperties": True
                            },
                            "context_id": {"type": "string", "description": "Context ID (optional)"},
                            "override_id": {"type": "string", "description": "Override ID (optional)"}
                        },
                        "required": ["id", "variant_type", "overrides"]
                    }
                },
                "description": {
                    "type": "string",
                    "description": "Description of the experiment"
                },
                "change_reason": {
                    "type": "string",
                    "description": "Reason for creating this experiment (required for audit)"
                },
                "metrics": {
                    "type": "object",
                    "description": "Metrics configuration for the experiment",
                    "additionalProperties": True
                },
                "experiment_group_id": {
                    "type": "string",
                    "description": "ID of the experiment group this experiment belongs to"
                }
            },
            "required": ["name", "context", "variants", "description", "change_reason"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return CreateExperimentSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the create experiment operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.create_experiment(
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            name=validated_input.name,
            experiment_type=validated_input.experiment_type,
            context=validated_input.context,
            variants=validated_input.variants,
            description=validated_input.description,
            change_reason=validated_input.change_reason,
            metrics=validated_input.metrics,
            experiment_group_id=validated_input.experiment_group_id
        )
        
        return self.format_success_response(result)


class GetExperimentTool(BaseTool):
    """Tool for retrieving experiment details."""
    
    @property
    def name(self) -> str:
        return "get_experiment"
    
    @property
    def description(self) -> str:
        return """Get details of a specific experiment by its ID.
        
        This tool retrieves complete information about an experiment including its variants,
        current status, metrics, and metadata.
        
        Examples:
        - Get experiment: {"experiment_id": "exp_abc123"}
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
                "experiment_id": {
                    "type": "string",
                    "description": "Unique identifier of the experiment to retrieve"
                }
            },
            "required": ["experiment_id"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return GetExperimentSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the get experiment operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.get_experiment(
            experiment_id=validated_input.experiment_id,
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id
        )
        
        return self.format_success_response(result)


class UpdateExperimentTool(BaseTool):
    """Tool for updating experiment variants."""
    
    @property
    def name(self) -> str:
        return "update_experiment"
    
    @property
    def description(self) -> str:
        return """Update the variants and configuration of an existing experiment.
        
        This allows you to modify the experiment's variants, update metrics configuration,
        or change other experiment properties.
        
        Examples:
        - Update variants: {
            "experiment_id": "exp_123",
            "variants": [
              {"id": "control", "overrides": {"timeout": 1000}},
              {"id": "variant_a", "overrides": {"timeout": 2000}}
            ],
            "change_reason": "Adjust timeout values"
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
                "experiment_id": {
                    "type": "string",
                    "description": "Unique identifier of the experiment to update"
                },
                "variants": {
                    "type": "array",
                    "description": "Updated list of experiment variants",
                    "items": {
                        "type": "object",
                        "properties": {
                            "id": {"type": "string", "description": "Unique variant ID"},
                            "overrides": {
                                "type": "object",
                                "description": "Configuration overrides for this variant",
                                "additionalProperties": True
                            }
                        },
                        "required": ["id", "overrides"]
                    }
                },
                "description": {
                    "type": "string",
                    "description": "Updated description for the experiment"
                },
                "change_reason": {
                    "type": "string",
                    "description": "Reason for updating this experiment (required for audit)"
                },
                "metrics": {
                    "type": "object",
                    "description": "Updated metrics configuration",
                    "additionalProperties": True
                },
                "experiment_group_id": {
                    "type": "string",
                    "description": "Updated experiment group ID"
                }
            },
            "required": ["experiment_id", "variants", "change_reason"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return UpdateExperimentSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the update experiment operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.update_experiment(
            experiment_id=validated_input.experiment_id,
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            variants=validated_input.variants,
            description=validated_input.description,
            change_reason=validated_input.change_reason,
            metrics=validated_input.metrics,
            experiment_group_id=validated_input.experiment_group_id
        )
        
        return self.format_success_response(result)


class ConcludeExperimentTool(BaseTool):
    """Tool for concluding experiments."""
    
    @property
    def name(self) -> str:
        return "conclude_experiment"
    
    @property
    def description(self) -> str:
        return """Conclude an experiment by selecting the winning variant.
        
        This permanently ends the experiment and promotes the chosen variant's configuration
        as the default for the experiment's context.
        
        Examples:
        - Conclude with winner: {
            "experiment_id": "exp_123",
            "chosen_variant": "variant_a",
            "change_reason": "Variant A showed 15% improvement"
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
                "experiment_id": {
                    "type": "string",
                    "description": "Unique identifier of the experiment to conclude"
                },
                "chosen_variant": {
                    "type": "string",
                    "description": "ID of the winning variant to promote"
                },
                "description": {
                    "type": "string",
                    "description": "Conclusion notes for the experiment"
                },
                "change_reason": {
                    "type": "string",
                    "description": "Reason for concluding this experiment (required for audit)"
                }
            },
            "required": ["experiment_id", "chosen_variant", "change_reason"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return ConcludeExperimentSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the conclude experiment operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.conclude_experiment(
            experiment_id=validated_input.experiment_id,
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            chosen_variant=validated_input.chosen_variant,
            description=validated_input.description,
            change_reason=validated_input.change_reason
        )
        
        return self.format_success_response(result)


class DiscardExperimentTool(BaseTool):
    """Tool for discarding experiments."""
    
    @property
    def name(self) -> str:
        return "discard_experiment"
    
    @property
    def description(self) -> str:
        return """Discard an experiment without promoting any variant.
        
        This ends the experiment without making any configuration changes. Use this when
        the experiment results are inconclusive or when you decide not to proceed.
        
        Examples:
        - Discard experiment: {
            "experiment_id": "exp_123",
            "change_reason": "Inconclusive results, no significant difference"
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
                "experiment_id": {
                    "type": "string",
                    "description": "Unique identifier of the experiment to discard"
                },
                "change_reason": {
                    "type": "string",
                    "description": "Reason for discarding this experiment (required for audit)"
                }
            },
            "required": ["experiment_id", "change_reason"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return DiscardExperimentSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the discard experiment operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.discard_experiment(
            experiment_id=validated_input.experiment_id,
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            change_reason=validated_input.change_reason
        )
        
        return self.format_success_response(result)


class RampExperimentTool(BaseTool):
    """Tool for ramping experiment traffic."""
    
    @property
    def name(self) -> str:
        return "ramp_experiment"
    
    @property
    def description(self) -> str:
        return """Adjust the traffic percentage for an experiment.
        
        This allows you to gradually increase or decrease the percentage of traffic
        that participates in the experiment.
        
        Examples:
        - Ramp up to 50%: {
            "experiment_id": "exp_123",
            "traffic_percentage": 50,
            "change_reason": "Gradually increase experiment exposure"
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
                "experiment_id": {
                    "type": "string",
                    "description": "Unique identifier of the experiment to ramp"
                },
                "traffic_percentage": {
                    "type": "integer",
                    "minimum": 0,
                    "maximum": 100,
                    "description": "New traffic percentage for the experiment"
                },
                "change_reason": {
                    "type": "string",
                    "description": "Reason for ramping this experiment (required for audit)"
                }
            },
            "required": ["experiment_id", "traffic_percentage", "change_reason"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return RampExperimentSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the ramp experiment operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.ramp_experiment(
            experiment_id=validated_input.experiment_id,
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            traffic_percentage=validated_input.traffic_percentage,
            change_reason=validated_input.change_reason
        )
        
        return self.format_success_response(result)


class PauseExperimentTool(BaseTool):
    """Tool for pausing experiments."""
    
    @property
    def name(self) -> str:
        return "pause_experiment"
    
    @property
    def description(self) -> str:
        return """Pause a running experiment.
        
        This temporarily stops the experiment without concluding it. The experiment
        can be resumed later using the resume_experiment tool.
        
        Examples:
        - Pause experiment: {
            "experiment_id": "exp_123",
            "change_reason": "Pause due to unexpected behavior"
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
                "experiment_id": {
                    "type": "string",
                    "description": "Unique identifier of the experiment to pause"
                },
                "change_reason": {
                    "type": "string",
                    "description": "Reason for pausing this experiment (required for audit)"
                }
            },
            "required": ["experiment_id", "change_reason"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return PauseExperimentSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the pause experiment operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.pause_experiment(
            experiment_id=validated_input.experiment_id,
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            change_reason=validated_input.change_reason
        )
        
        return self.format_success_response(result)


class ResumeExperimentTool(BaseTool):
    """Tool for resuming paused experiments."""
    
    @property
    def name(self) -> str:
        return "resume_experiment"
    
    @property
    def description(self) -> str:
        return """Resume a paused experiment.
        
        This restarts a previously paused experiment, returning it to active status.
        
        Examples:
        - Resume experiment: {
            "experiment_id": "exp_123",
            "change_reason": "Issue resolved, resuming experiment"
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
                "experiment_id": {
                    "type": "string",
                    "description": "Unique identifier of the experiment to resume"
                },
                "change_reason": {
                    "type": "string",
                    "description": "Reason for resuming this experiment (required for audit)"
                }
            },
            "required": ["experiment_id", "change_reason"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return ResumeExperimentSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the resume experiment operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.resume_experiment(
            experiment_id=validated_input.experiment_id,
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            change_reason=validated_input.change_reason
        )
        
        return self.format_success_response(result)


class ListExperimentsTool(BaseTool):
    """Tool for listing experiments with filtering."""
    
    @property
    def name(self) -> str:
        return "list_experiments"
    
    @property
    def description(self) -> str:
        return """List experiments with optional filtering and pagination.
        
        This tool allows you to retrieve experiments with various filtering options like
        status, date range, creator, and name matching. Results are paginated and sortable.
        
        Examples:
        - List all: {}
        - Filter by status: {"status": "INPROGRESS"}
        - Date range: {"from_date": "2024-01-01T00:00:00Z", "to_date": "2024-12-31T23:59:59Z"}
        - Search by name: {"experiment_name": "button"}
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
                    "description": "Number of experiments to retrieve per page"
                },
                "all": {
                    "type": "boolean",
                    "description": "Retrieve all experiments (ignores pagination)"
                },
                "status": {
                    "type": "string",
                    "enum": ["CREATED", "INPROGRESS", "CONCLUDED", "DISCARDED", "PAUSED"],
                    "description": "Filter by experiment status"
                },
                "from_date": {
                    "type": "string",
                    "format": "date-time",
                    "description": "Filter experiments created after this date (ISO format)"
                },
                "to_date": {
                    "type": "string",
                    "format": "date-time", 
                    "description": "Filter experiments created before this date (ISO format)"
                },
                "experiment_name": {
                    "type": "string",
                    "description": "Filter by experiment name (partial match)"
                },
                "experiment_ids": {
                    "type": "string",
                    "description": "Comma-separated list of experiment IDs to filter"
                },
                "experiment_group_ids": {
                    "type": "string",
                    "description": "Comma-separated list of experiment group IDs to filter"
                },
                "created_by": {
                    "type": "string",
                    "description": "Filter by experiment creator"
                },
                "sort_on": {
                    "type": "string",
                    "enum": ["last_modified_at", "created_at"],
                    "description": "Field to sort by"
                },
                "sort_by": {
                    "type": "string",
                    "enum": ["ASC", "DESC"],
                    "description": "Sort direction"
                }
            },
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return ListExperimentsSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the list experiments operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.list_experiments(
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            page=validated_input.page,
            count=validated_input.count,
            all=validated_input.all,
            status=validated_input.status,
            from_date=validated_input.from_date,
            to_date=validated_input.to_date,
            experiment_name=validated_input.experiment_name,
            experiment_ids=validated_input.experiment_ids,
            experiment_group_ids=validated_input.experiment_group_ids,
            created_by=validated_input.created_by,
            sort_on=validated_input.sort_on,
            sort_by=validated_input.sort_by
        )
        
        return self.format_success_response(result)


class ApplicableVariantsTool(BaseTool):
    """Tool for getting applicable experiment variants."""
    
    @property
    def name(self) -> str:
        return "get_applicable_variants"
    
    @property
    def description(self) -> str:
        return """Get applicable experiment variants for a given context.
        
        This tool determines which experiment variants would apply to a specific context
        and user, taking into account the random toss value for variant assignment.
        
        Examples:
        - Check variants: {
            "context": {"user_type": "premium", "region": "us-east"},
            "toss": 42
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
                    "description": "Context to evaluate for applicable variants",
                    "additionalProperties": True
                },
                "toss": {
                    "type": "integer",
                    "minimum": 0,
                    "maximum": 100,
                    "description": "Random toss value for determining variant assignment"
                }
            },
            "required": ["context", "toss"],
            "additionalProperties": False
        }
    
    @property
    def schema_class(self):
        return ApplicableVariantsSchema
    
    async def execute(self, arguments: Dict[str, Any]) -> list[TextContent]:
        """Execute the get applicable variants operation."""
        validated_input = self.validate_input(arguments)
        
        result = await self.client.get_applicable_variants(
            workspace_id=validated_input.workspace_id,
            org_id=validated_input.org_id,
            context=validated_input.context,
            toss=validated_input.toss
        )
        
        return self.format_success_response(result)