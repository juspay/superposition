#!/usr/bin/env python3
"""
Usage examples for Superposition MCP Server tools.
These examples demonstrate how AI assistants can use the available tools.
"""

import asyncio
import json
from typing import Dict, Any


# Example 1: Basic Configuration Management
async def example_configuration_management():
    """Example of managing configurations with the MCP server."""
    
    # Create a default configuration
    config_result = await create_default_config({
        "workspace_id": "dev",
        "org_id": "juspay",
        "key": "feature_flags.new_ui",
        "value": {
            "enabled": False,
            "rollout_percentage": 0
        },
        "schema": {
            "type": "object",
            "properties": {
                "enabled": {"type": "boolean"},
                "rollout_percentage": {"type": "integer", "minimum": 0, "maximum": 100}
            },
            "required": ["enabled", "rollout_percentage"]
        },
        "description": "Feature flag for new UI components",
        "change_reason": "Initial setup for new UI rollout"
    })
    
    print("Created default config:", json.dumps(config_result, indent=2))
    
    # Get configuration with context
    contextual_config = await get_config({
        "workspace_id": "dev",
        "org_id": "juspay",
        "context": {
            "user_type": "premium",
            "region": "us-west",
            "beta_user": True
        }
    })
    
    print("Contextual config:", json.dumps(contextual_config, indent=2))


# Example 2: Context-Aware Overrides
async def example_context_overrides():
    """Example of creating context-specific overrides."""
    
    # Create a context for beta users
    beta_context = await create_context({
        "workspace_id": "dev",
        "org_id": "juspay",
        "context": {
            "and": [
                {"var": "beta_user", "==": True},
                {"var": "user_type", "in": ["premium", "enterprise"]}
            ]
        },
        "override": {
            "feature_flags.new_ui": {
                "enabled": True,
                "rollout_percentage": 100
            }
        },
        "description": "Enable new UI for premium beta users",
        "change_reason": "Beta testing phase"
    })
    
    print("Created beta context:", json.dumps(beta_context, indent=2))
    
    # Create a context for specific regions
    regional_context = await create_context({
        "workspace_id": "dev",
        "org_id": "juspay",
        "context": {
            "var": "region",
            "in": ["us-west", "eu-west"]
        },
        "override": {
            "feature_flags.new_ui": {
                "enabled": True,
                "rollout_percentage": 25
            }
        },
        "description": "Gradual rollout in western regions",
        "change_reason": "Phased rollout strategy"
    })
    
    print("Created regional context:", json.dumps(regional_context, indent=2))


# Example 3: A/B Testing Experiment
async def example_ab_testing():
    """Example of setting up an A/B testing experiment."""
    
    # Create an experiment for testing new checkout flow
    experiment = await create_experiment({
        "workspace_id": "dev",
        "org_id": "juspay",
        "name": "Checkout Flow Optimization",
        "context": {
            "and": [
                {"var": "user_type", "in": ["premium", "standard"]},
                {"var": "region", "!=": "restricted"}
            ]
        },
        "variants": [
            {
                "id": "control",
                "variant_type": "CONTROL",
                "overrides": {
                    "checkout.flow": "classic",
                    "checkout.steps": 4,
                    "checkout.validation": "standard"
                }
            },
            {
                "id": "streamlined",
                "variant_type": "EXPERIMENTAL",
                "overrides": {
                    "checkout.flow": "streamlined",
                    "checkout.steps": 2,
                    "checkout.validation": "enhanced"
                }
            },
            {
                "id": "single_page",
                "variant_type": "EXPERIMENTAL",
                "overrides": {
                    "checkout.flow": "single_page",
                    "checkout.steps": 1,
                    "checkout.validation": "smart"
                }
            }
        ],
        "description": "Testing different checkout flows to improve conversion rates",
        "change_reason": "Conversion rate optimization initiative",
        "metrics": {
            "primary": "conversion_rate",
            "secondary": ["completion_time", "error_rate", "user_satisfaction"]
        }
    })
    
    print("Created experiment:", json.dumps(experiment, indent=2))
    
    # Get applicable variants for a specific user
    applicable_variants = await applicable_variants({
        "workspace_id": "dev",
        "org_id": "juspay",
        "context": {
            "user_id": "user_12345",
            "user_type": "premium",
            "region": "us-east"
        },
        "toss": 42  # Random seed for consistent assignment
    })
    
    print("Applicable variants:", json.dumps(applicable_variants, indent=2))


# Example 4: Experiment Group Management
async def example_experiment_groups():
    """Example of managing experiment groups."""
    
    # Create an experiment group for related experiments
    experiment_group = await create_experiment_group({
        "workspace_id": "dev",
        "org_id": "juspay",
        "name": "Q1 2025 Conversion Optimization",
        "description": "Collection of experiments focused on improving conversion rates",
        "priority": 1,
        "traffic_percentage": 50
    })
    
    print("Created experiment group:", json.dumps(experiment_group, indent=2))
    
    # Add existing experiments to the group
    add_result = await add_members_to_group({
        "workspace_id": "dev",
        "org_id": "juspay",
        "group_id": experiment_group["id"],
        "experiment_ids": ["exp_1", "exp_2", "exp_3"]
    })
    
    print("Added experiments to group:", json.dumps(add_result, indent=2))


# Example 5: Configuration Validation and Functions
async def example_validation_functions():
    """Example of creating validation functions for configurations."""
    
    # Create a validation function for email addresses
    email_function = await create_function({
        "workspace_id": "dev",
        "org_id": "juspay",
        "function_name": "validate_email",
        "description": "Validates email address format",
        "function": """
function validateEmail(email) {
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    return emailRegex.test(email);
}
""",
        "published": True
    })
    
    print("Created validation function:", json.dumps(email_function, indent=2))
    
    # Create a configuration that uses the validation function
    config_with_validation = await create_default_config({
        "workspace_id": "dev",
        "org_id": "juspay",
        "key": "user.contact_email",
        "value": "user@example.com",
        "schema": {
            "type": "string",
            "format": "email"
        },
        "function_name": "validate_email",
        "description": "Default contact email for users",
        "change_reason": "Setup user contact configuration"
    })
    
    print("Created config with validation:", json.dumps(config_with_validation, indent=2))


# Example 6: Dimension Management
async def example_dimension_management():
    """Example of managing configuration dimensions."""
    
    # Create a dimension for user segments
    user_segment_dimension = await create_dimension({
        "workspace_id": "dev",
        "org_id": "juspay",
        "dimension": "user_segment",
        "priority": 10,
        "description": "User segmentation based on behavior and value",
        "schema": {
            "type": "string",
            "enum": ["new", "active", "premium", "enterprise", "churned"]
        },
        "function_name": "calculate_user_segment"
    })
    
    print("Created dimension:", json.dumps(user_segment_dimension, indent=2))
    
    # List all dimensions
    dimensions = await list_dimensions({
        "workspace_id": "dev",
        "org_id": "juspay"
    })
    
    print("All dimensions:", json.dumps(dimensions, indent=2))


# Example 7: Audit and Monitoring
async def example_audit_monitoring():
    """Example of accessing audit logs and monitoring."""
    
    # Get audit logs for recent changes
    audit_logs = await list_audit_logs({
        "workspace_id": "dev",
        "org_id": "juspay",
        "page": 1,
        "count": 50,
        "from_date": "2025-01-01T00:00:00Z",
        "to_date": "2025-01-31T23:59:59Z"
    })
    
    print("Recent audit logs:", json.dumps(audit_logs, indent=2))
    
    # Test connectivity
    test_result = await test({
        "workspace_id": "dev",
        "org_id": "juspay"
    })
    
    print("Connectivity test:", json.dumps(test_result, indent=2))


# Example 8: Workspace and Organization Management
async def example_workspace_management():
    """Example of managing workspaces and organizations."""
    
    # Create a new workspace for staging
    staging_workspace = await create_workspace({
        "org_id": "juspay",
        "workspace_name": "staging",
        "description": "Staging environment for testing changes"
    })
    
    print("Created staging workspace:", json.dumps(staging_workspace, indent=2))
    
    # List all workspaces
    workspaces = await list_workspace({
        "org_id": "juspay"
    })
    
    print("All workspaces:", json.dumps(workspaces, indent=2))


# Example 9: Complex Configuration Resolution
async def example_complex_resolution():
    """Example of complex configuration resolution with multiple contexts."""
    
    # Get resolved configuration with complex context
    resolved_config = await get_resolved_config({
        "workspace_id": "dev",
        "org_id": "juspay",
        "context": {
            "user_id": "user_67890",
            "user_type": "premium",
            "region": "eu-west",
            "device_type": "mobile",
            "beta_user": True,
            "subscription_tier": "pro",
            "feature_flags": ["new_ui", "advanced_analytics"]
        },
        "merge_strategy": "MERGE",
        "show_reasoning": True
    })
    
    print("Resolved configuration:", json.dumps(resolved_config, indent=2))


# Example 10: Experiment Lifecycle Management
async def example_experiment_lifecycle():
    """Example of managing the complete experiment lifecycle."""
    
    # Start with creating an experiment (from example 3)
    experiment_id = "exp_checkout_flow_123"
    
    # Ramp up traffic gradually
    ramp_result = await ramp_experiment({
        "workspace_id": "dev",
        "org_id": "juspay",
        "id": experiment_id,
        "traffic_percentage": 25,
        "change_reason": "Initial ramp up to 25% traffic"
    })
    
    print("Ramped experiment:", json.dumps(ramp_result, indent=2))
    
    # Later, increase traffic further
    ramp_result_2 = await ramp_experiment({
        "workspace_id": "dev",
        "org_id": "juspay",
        "id": experiment_id,
        "traffic_percentage": 75,
        "change_reason": "Increased to 75% after positive initial results"
    })
    
    print("Further ramped experiment:", json.dumps(ramp_result_2, indent=2))
    
    # Conclude the experiment with winning variant
    conclude_result = await conclude_experiment({
        "workspace_id": "dev",
        "org_id": "juspay",
        "id": experiment_id,
        "chosen_variant": "streamlined",
        "change_reason": "Streamlined flow showed 15% improvement in conversion rate",
        "description": "Experiment concluded successfully with significant conversion improvement"
    })
    
    print("Concluded experiment:", json.dumps(conclude_result, indent=2))


# Utility functions (these would be the actual MCP tool calls)
async def create_default_config(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for creating default config."""
    return {"success": True, "data": params, "id": "config_123"}

async def get_config(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for getting config."""
    return {"success": True, "data": {"config": {}, "contexts": []}}

async def create_context(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for creating context."""
    return {"success": True, "data": params, "context_id": "ctx_123"}

async def create_experiment(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for creating experiment."""
    return {"success": True, "data": params, "id": "exp_123"}

async def applicable_variants(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for getting applicable variants."""
    return {"success": True, "data": [{"id": "control", "variant_type": "CONTROL"}]}

async def create_experiment_group(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for creating experiment group."""
    return {"success": True, "data": params, "id": "group_123"}

async def add_members_to_group(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for adding members to group."""
    return {"success": True, "data": params}

async def create_function(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for creating function."""
    return {"success": True, "data": params, "function_name": params["function_name"]}

async def create_dimension(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for creating dimension."""
    return {"success": True, "data": params, "dimension": params["dimension"]}

async def list_dimensions(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for listing dimensions."""
    return {"success": True, "data": []}

async def list_audit_logs(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for listing audit logs."""
    return {"success": True, "data": []}

async def test(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for testing connectivity."""
    return {"success": True, "status": "healthy"}

async def create_workspace(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for creating workspace."""
    return {"success": True, "data": params}

async def list_workspace(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for listing workspaces."""
    return {"success": True, "data": []}

async def get_resolved_config(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for getting resolved config."""
    return {"success": True, "data": {"config": {}, "reasoning": []}}

async def ramp_experiment(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for ramping experiment."""
    return {"success": True, "data": params}

async def conclude_experiment(params: Dict[str, Any]) -> Dict[str, Any]:
    """Simulate MCP tool call for concluding experiment."""
    return {"success": True, "data": params}


# Main function to run examples
async def main():
    """Run all examples."""
    print("=== Superposition MCP Server Usage Examples ===\n")
    
    examples = [
        ("Configuration Management", example_configuration_management),
        ("Context Overrides", example_context_overrides),
        ("A/B Testing", example_ab_testing),
        ("Experiment Groups", example_experiment_groups),
        ("Validation Functions", example_validation_functions),
        ("Dimension Management", example_dimension_management),
        ("Audit and Monitoring", example_audit_monitoring),
        ("Workspace Management", example_workspace_management),
        ("Complex Resolution", example_complex_resolution),
        ("Experiment Lifecycle", example_experiment_lifecycle),
    ]
    
    for name, example_func in examples:
        print(f"\n{'='*50}")
        print(f"Example: {name}")
        print('='*50)
        try:
            await example_func()
        except Exception as e:
            print(f"Error in {name}: {e}")
        print("\n" + "."*50)


if __name__ == "__main__":
    asyncio.run(main())