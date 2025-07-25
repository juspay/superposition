"""
Mock implementations for Superposition SDK components used in testing.
"""

import asyncio
from typing import Any, Dict, List, Optional, Union
from unittest.mock import AsyncMock, MagicMock

from superposition_sdk.models import (
    GetConfigInput, GetConfigOutput,
    CreateDefaultConfigInput, CreateDefaultConfigOutput,
    UpdateDefaultConfigInput, UpdateDefaultConfigOutput,
    DeleteDefaultConfigInput, DeleteDefaultConfigOutput,
    ListDefaultConfigsInput, ListDefaultConfigsOutput,
    CreateContextInput, CreateContextOutput,
    GetContextInput, GetContextOutput,
    UpdateOverrideInput, UpdateOverrideOutput,
    DeleteContextInput, DeleteContextOutput,
    ListContextsInput, ListContextsOutput,
    CreateExperimentInput, CreateExperimentOutput,
    GetExperimentInput, GetExperimentOutput,
    ListExperimentInput, ListExperimentOutput,
    UpdateOverridesExperimentInput, UpdateOverridesExperimentOutput,
    RampExperimentInput, RampExperimentOutput,
    PauseExperimentInput, PauseExperimentOutput,
    ResumeExperimentInput, ResumeExperimentOutput,
    ConcludeExperimentInput, ConcludeExperimentOutput,
    CreateExperimentGroupInput, CreateExperimentGroupOutput,
    GetExperimentGroupInput, GetExperimentGroupOutput,
    ListExperimentGroupsInput, ListExperimentGroupsOutput,
)


class MockSuperpositionClient:
    """Mock implementation of Superposition client for testing."""
    
    def __init__(self, should_fail: bool = False, failure_mode: str = "network"):
        self.should_fail = should_fail
        self.failure_mode = failure_mode
        self.call_history = []
        
        # Mock data stores
        self.configs = {}
        self.contexts = {}
        self.experiments = {}
        self.experiment_groups = {}
    
    def _record_call(self, method: str, **kwargs):
        """Record method calls for testing verification."""
        self.call_history.append({
            'method': method,
            'args': kwargs,
            'timestamp': asyncio.get_event_loop().time()
        })
    
    def _simulate_failure(self):
        """Simulate different types of failures."""
        if not self.should_fail:
            return
        
        if self.failure_mode == "network":
            raise ConnectionError("Connection timeout")
        elif self.failure_mode == "auth":
            raise PermissionError("Authentication failed")
        elif self.failure_mode == "validation":
            raise ValueError("Invalid input parameters")
        elif self.failure_mode == "server":
            raise RuntimeError("Internal server error")
        else:
            raise Exception(f"Unknown failure mode: {self.failure_mode}")
    
    # CAC Methods
    async def get_config(self, input_data: GetConfigInput) -> GetConfigOutput:
        """Mock get_config operation."""
        self._record_call("get_config", tenant=input_data.tenant, key=input_data.key)
        self._simulate_failure()
        
        config_key = f"{input_data.tenant}:{input_data.key}"
        if config_key in self.configs:
            return GetConfigOutput(value=self.configs[config_key])
        else:
            # Return empty result for missing configs
            return GetConfigOutput(value={})
    
    async def create_default_config(self, input_data: CreateDefaultConfigInput) -> CreateDefaultConfigOutput:
        """Mock create_default_config operation."""
        self._record_call("create_default_config", 
                         tenant=input_data.tenant, 
                         key=input_data.key,
                         value=input_data.value)
        self._simulate_failure()
        
        config_key = f"{input_data.tenant}:{input_data.key}"
        self.configs[config_key] = input_data.value
        
        return CreateDefaultConfigOutput()
    
    async def update_default_config(self, input_data: UpdateDefaultConfigInput) -> UpdateDefaultConfigOutput:
        """Mock update_default_config operation."""
        self._record_call("update_default_config",
                         tenant=input_data.tenant,
                         key=input_data.key,
                         value=input_data.value)
        self._simulate_failure()
        
        config_key = f"{input_data.tenant}:{input_data.key}"
        if config_key in self.configs:
            self.configs[config_key] = input_data.value
        
        return UpdateDefaultConfigOutput()
    
    async def delete_default_config(self, input_data: DeleteDefaultConfigInput) -> DeleteDefaultConfigOutput:
        """Mock delete_default_config operation."""
        self._record_call("delete_default_config",
                         tenant=input_data.tenant,
                         key=input_data.key)
        self._simulate_failure()
        
        config_key = f"{input_data.tenant}:{input_data.key}"
        if config_key in self.configs:
            del self.configs[config_key]
        
        return DeleteDefaultConfigOutput()
    
    async def list_default_configs(self, input_data: ListDefaultConfigsInput) -> ListDefaultConfigsOutput:
        """Mock list_default_configs operation."""
        self._record_call("list_default_configs", tenant=input_data.tenant)
        self._simulate_failure()
        
        tenant_configs = {
            key.split(":", 1)[1]: value 
            for key, value in self.configs.items() 
            if key.startswith(f"{input_data.tenant}:")
        }
        
        return ListDefaultConfigsOutput(configs=tenant_configs)
    
    async def create_context(self, input_data: CreateContextInput) -> CreateContextOutput:
        """Mock create_context operation."""
        self._record_call("create_context",
                         tenant=input_data.tenant,
                         context_id=input_data.context_id)
        self._simulate_failure()
        
        context_key = f"{input_data.tenant}:{input_data.context_id}"
        self.contexts[context_key] = {
            'condition': input_data.condition,
            'override': input_data.override,
            'priority': input_data.priority
        }
        
        return CreateContextOutput()
    
    async def get_context(self, input_data: GetContextInput) -> GetContextOutput:
        """Mock get_context operation."""
        self._record_call("get_context",
                         tenant=input_data.tenant,
                         context_id=input_data.context_id)
        self._simulate_failure()
        
        context_key = f"{input_data.tenant}:{input_data.context_id}"
        if context_key in self.contexts:
            return GetContextOutput(**self.contexts[context_key])
        else:
            raise ValueError(f"Context not found: {input_data.context_id}")
    
    async def update_override(self, input_data: UpdateOverrideInput) -> UpdateOverrideOutput:
        """Mock update_override operation."""
        self._record_call("update_override",
                         tenant=input_data.tenant,
                         context_id=input_data.context_id)
        self._simulate_failure()
        
        context_key = f"{input_data.tenant}:{input_data.context_id}"
        if context_key in self.contexts:
            self.contexts[context_key]['override'] = input_data.override
        
        return UpdateOverrideOutput()
    
    async def delete_context(self, input_data: DeleteContextInput) -> DeleteContextOutput:
        """Mock delete_context operation."""
        self._record_call("delete_context",
                         tenant=input_data.tenant,
                         context_id=input_data.context_id)
        self._simulate_failure()
        
        context_key = f"{input_data.tenant}:{input_data.context_id}"
        if context_key in self.contexts:
            del self.contexts[context_key]
        
        return DeleteContextOutput()
    
    async def list_contexts(self, input_data: ListContextsInput) -> ListContextsOutput:
        """Mock list_contexts operation."""
        self._record_call("list_contexts", tenant=input_data.tenant)
        self._simulate_failure()
        
        tenant_contexts = [
            {
                'context_id': key.split(":", 1)[1],
                **value
            }
            for key, value in self.contexts.items()
            if key.startswith(f"{input_data.tenant}:")
        ]
        
        return ListContextsOutput(contexts=tenant_contexts)
    
    # Experimentation Methods
    async def create_experiment(self, input_data: CreateExperimentInput) -> CreateExperimentOutput:
        """Mock create_experiment operation."""
        self._record_call("create_experiment",
                         tenant=input_data.tenant,
                         experiment_name=input_data.experiment_name)
        self._simulate_failure()
        
        experiment_key = f"{input_data.tenant}:{input_data.experiment_name}"
        experiment_id = f"exp_{len(self.experiments) + 1}"
        
        self.experiments[experiment_key] = {
            'experiment_id': experiment_id,
            'experiment_name': input_data.experiment_name,
            'context': input_data.context,
            'variants': input_data.variants,
            'overrides': input_data.overrides,
            'traffic_percentage': input_data.traffic_percentage,
            'status': 'CREATED'
        }
        
        return CreateExperimentOutput(experiment_id=experiment_id)
    
    async def get_experiment(self, input_data: GetExperimentInput) -> GetExperimentOutput:
        """Mock get_experiment operation."""
        self._record_call("get_experiment",
                         tenant=input_data.tenant,
                         experiment_id=input_data.experiment_id)
        self._simulate_failure()
        
        # Find experiment by ID
        for exp_data in self.experiments.values():
            if exp_data['experiment_id'] == input_data.experiment_id:
                return GetExperimentOutput(**exp_data)
        
        raise ValueError(f"Experiment not found: {input_data.experiment_id}")
    
    async def list_experiment(self, input_data: ListExperimentInput) -> ListExperimentOutput:
        """Mock list_experiment operation."""
        self._record_call("list_experiment", tenant=input_data.tenant)
        self._simulate_failure()
        
        tenant_experiments = [
            exp_data for key, exp_data in self.experiments.items()
            if key.startswith(f"{input_data.tenant}:")
        ]
        
        return ListExperimentOutput(experiments=tenant_experiments)
    
    async def update_overrides_experiment(self, input_data: UpdateOverridesExperimentInput) -> UpdateOverridesExperimentOutput:
        """Mock update_overrides_experiment operation."""
        self._record_call("update_overrides_experiment",
                         tenant=input_data.tenant,
                         experiment_id=input_data.experiment_id)
        self._simulate_failure()
        
        # Find and update experiment
        for exp_data in self.experiments.values():
            if exp_data['experiment_id'] == input_data.experiment_id:
                exp_data['overrides'] = input_data.overrides
                break
        
        return UpdateOverridesExperimentOutput()
    
    async def ramp_experiment(self, input_data: RampExperimentInput) -> RampExperimentOutput:
        """Mock ramp_experiment operation."""
        self._record_call("ramp_experiment",
                         tenant=input_data.tenant,
                         experiment_id=input_data.experiment_id)
        self._simulate_failure()
        
        # Find and update experiment status
        for exp_data in self.experiments.values():
            if exp_data['experiment_id'] == input_data.experiment_id:
                exp_data['traffic_percentage'] = input_data.traffic_percentage
                exp_data['status'] = 'RUNNING'
                break
        
        return RampExperimentOutput()
    
    async def pause_experiment(self, input_data: PauseExperimentInput) -> PauseExperimentOutput:
        """Mock pause_experiment operation."""
        self._record_call("pause_experiment",
                         tenant=input_data.tenant,
                         experiment_id=input_data.experiment_id)
        self._simulate_failure()
        
        # Find and update experiment status
        for exp_data in self.experiments.values():
            if exp_data['experiment_id'] == input_data.experiment_id:
                exp_data['status'] = 'PAUSED'
                break
        
        return PauseExperimentOutput()
    
    async def resume_experiment(self, input_data: ResumeExperimentInput) -> ResumeExperimentOutput:
        """Mock resume_experiment operation."""
        self._record_call("resume_experiment",
                         tenant=input_data.tenant,
                         experiment_id=input_data.experiment_id)
        self._simulate_failure()
        
        # Find and update experiment status
        for exp_data in self.experiments.values():
            if exp_data['experiment_id'] == input_data.experiment_id:
                exp_data['status'] = 'RUNNING'
                break
        
        return ResumeExperimentOutput()
    
    async def conclude_experiment(self, input_data: ConcludeExperimentInput) -> ConcludeExperimentOutput:
        """Mock conclude_experiment operation."""
        self._record_call("conclude_experiment",
                         tenant=input_data.tenant,
                         experiment_id=input_data.experiment_id)
        self._simulate_failure()
        
        # Find and update experiment status
        for exp_data in self.experiments.values():
            if exp_data['experiment_id'] == input_data.experiment_id:
                exp_data['status'] = 'CONCLUDED'
                break
        
        return ConcludeExperimentOutput()
    
    async def create_experiment_group(self, input_data: CreateExperimentGroupInput) -> CreateExperimentGroupOutput:
        """Mock create_experiment_group operation."""
        self._record_call("create_experiment_group",
                         tenant=input_data.tenant,
                         group_name=input_data.group_name)
        self._simulate_failure()
        
        group_key = f"{input_data.tenant}:{input_data.group_name}"
        group_id = f"group_{len(self.experiment_groups) + 1}"
        
        self.experiment_groups[group_key] = {
            'group_id': group_id,
            'group_name': input_data.group_name,
            'description': getattr(input_data, 'description', ''),
            'members': getattr(input_data, 'members', [])
        }
        
        return CreateExperimentGroupOutput(group_id=group_id)
    
    async def get_experiment_group(self, input_data: GetExperimentGroupInput) -> GetExperimentGroupOutput:
        """Mock get_experiment_group operation."""
        self._record_call("get_experiment_group",
                         tenant=input_data.tenant,
                         group_id=input_data.group_id)
        self._simulate_failure()
        
        # Find group by ID
        for group_data in self.experiment_groups.values():
            if group_data['group_id'] == input_data.group_id:
                return GetExperimentGroupOutput(**group_data)
        
        raise ValueError(f"Experiment group not found: {input_data.group_id}")
    
    async def list_experiment_groups(self, input_data: ListExperimentGroupsInput) -> ListExperimentGroupsOutput:
        """Mock list_experiment_groups operation."""
        self._record_call("list_experiment_groups", tenant=input_data.tenant)
        self._simulate_failure()
        
        tenant_groups = [
            group_data for key, group_data in self.experiment_groups.items()
            if key.startswith(f"{input_data.tenant}:")
        ]
        
        return ListExperimentGroupsOutput(groups=tenant_groups)
    
    def reset(self):
        """Reset mock state for clean testing."""
        self.call_history.clear()
        self.configs.clear()
        self.contexts.clear()
        self.experiments.clear()
        self.experiment_groups.clear()
        self.should_fail = False
        self.failure_mode = "network"
    
    def set_failure_mode(self, should_fail: bool, mode: str = "network"):
        """Configure failure simulation."""
        self.should_fail = should_fail
        self.failure_mode = mode
    
    def get_call_count(self, method: str) -> int:
        """Get number of times a method was called."""
        return len([call for call in self.call_history if call['method'] == method])
    
    def get_last_call(self, method: str) -> Optional[Dict]:
        """Get the last call to a specific method."""
        calls = [call for call in self.call_history if call['method'] == method]
        return calls[-1] if calls else None


class MockMCPServer:
    """Mock MCP server for testing server functionality."""
    
    def __init__(self):
        self.tools = {}
        self.is_running = False
        self.connections = []
        self.call_history = []
    
    def register_tool(self, name: str, handler):
        """Register a tool with the server."""
        self.tools[name] = handler
    
    def get_tool(self, name: str):
        """Get a registered tool."""
        return self.tools.get(name)
    
    def list_tools(self) -> List[str]:
        """List all registered tools."""
        return list(self.tools.keys())
    
    async def call_tool(self, name: str, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """Call a tool with given arguments."""
        self.call_history.append({
            'tool': name,
            'arguments': arguments,
            'timestamp': asyncio.get_event_loop().time()
        })
        
        if name not in self.tools:
            raise ValueError(f"Tool not found: {name}")
        
        tool = self.tools[name]
        return await tool(arguments)
    
    async def start(self, port: int = 8081):
        """Start the mock server."""
        self.is_running = True
    
    async def stop(self):
        """Stop the mock server."""
        self.is_running = False
        self.connections.clear()
    
    def reset(self):
        """Reset server state for testing."""
        self.call_history.clear()
        self.connections.clear()


# Factory functions for creating mock instances
def create_mock_client(should_fail: bool = False, failure_mode: str = "network") -> MockSuperpositionClient:
    """Create a mock Superposition client."""
    return MockSuperpositionClient(should_fail=should_fail, failure_mode=failure_mode)


def create_mock_server() -> MockMCPServer:
    """Create a mock MCP server."""
    return MockMCPServer()


# Test data generators
class MockDataGenerator:
    """Generate mock data for testing."""
    
    @staticmethod
    def generate_config_data(key: str = "test_key", value: Any = "test_value") -> Dict[str, Any]:
        return {
            'key': key,
            'value': value,
            'schema': {
                'type': 'string' if isinstance(value, str) else 'object'
            }
        }
    
    @staticmethod
    def generate_context_data(context_id: str = "test_context") -> Dict[str, Any]:
        return {
            'context_id': context_id,
            'condition': {'==': [{'var': 'user_id'}, 'test_user']},
            'override': {'test_key': 'overridden_value'},
            'priority': 10
        }
    
    @staticmethod
    def generate_experiment_data(name: str = "test_experiment") -> Dict[str, Any]:
        return {
            'experiment_name': name,
            'context': {'==': [{'var': 'segment'}, 'premium']},
            'variants': [
                {'id': 'control', 'weight': 50},
                {'id': 'treatment', 'weight': 50}
            ],
            'overrides': {
                'control': {'feature': 'off'},
                'treatment': {'feature': 'on'}
            },
            'traffic_percentage': 100
        }