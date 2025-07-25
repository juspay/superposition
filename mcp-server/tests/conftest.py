"""
Pytest configuration and shared fixtures for Superposition MCP Server tests.
"""

import asyncio
import json
import os
from typing import Any, Dict, List, Optional
from unittest.mock import AsyncMock, MagicMock, patch

import pytest
from superposition_sdk import Superposition
from superposition_sdk.config import Config


@pytest.fixture(scope="session")
def event_loop():
    """Create an instance of the default event loop for the test session."""
    loop = asyncio.new_event_loop()
    yield loop
    loop.close()


@pytest.fixture
def mock_superposition_client():
    """Mock Superposition SDK client for testing."""
    client = MagicMock(spec=Superposition)
    
    # Mock all the async methods
    async def mock_async_method(*args, **kwargs):
        return MagicMock()
    
    # CAC methods
    client.get_config = AsyncMock(return_value=MagicMock())
    client.create_default_config = AsyncMock(return_value=MagicMock())
    client.update_default_config = AsyncMock(return_value=MagicMock())
    client.delete_default_config = AsyncMock(return_value=MagicMock())
    client.list_default_configs = AsyncMock(return_value=MagicMock())
    client.create_context = AsyncMock(return_value=MagicMock())
    client.get_context = AsyncMock(return_value=MagicMock())
    client.update_override = AsyncMock(return_value=MagicMock())
    client.delete_context = AsyncMock(return_value=MagicMock())
    client.list_contexts = AsyncMock(return_value=MagicMock())
    
    # Experimentation methods
    client.create_experiment = AsyncMock(return_value=MagicMock())
    client.get_experiment = AsyncMock(return_value=MagicMock())
    client.list_experiment = AsyncMock(return_value=MagicMock())
    client.update_overrides_experiment = AsyncMock(return_value=MagicMock())
    client.ramp_experiment = AsyncMock(return_value=MagicMock())
    client.pause_experiment = AsyncMock(return_value=MagicMock())
    client.resume_experiment = AsyncMock(return_value=MagicMock())
    client.conclude_experiment = AsyncMock(return_value=MagicMock())
    client.create_experiment_group = AsyncMock(return_value=MagicMock())
    client.get_experiment_group = AsyncMock(return_value=MagicMock())
    client.list_experiment_groups = AsyncMock(return_value=MagicMock())
    
    return client


@pytest.fixture
def mock_config():
    """Mock configuration for testing."""
    return {
        'endpoint': 'http://localhost:8080',
        'tenant': 'test',
        'auth_token': 'test-token',
        'timeout': 30
    }


@pytest.fixture
def sample_config_data():
    """Sample configuration data for testing."""
    return {
        'key': 'test_feature_flag',
        'value': {'enabled': True, 'rollout_percentage': 50},
        'schema': {
            'type': 'object',
            'properties': {
                'enabled': {'type': 'boolean'},
                'rollout_percentage': {'type': 'number', 'minimum': 0, 'maximum': 100}
            }
        }
    }


@pytest.fixture
def sample_context_data():
    """Sample context data for testing."""
    return {
        'context_id': 'test_context',
        'condition': {
            'and': [
                {'==': [{'var': 'user_id'}, 'test_user']},
                {'>=': [{'var': 'version'}, '1.0.0']}
            ]
        },
        'priority': 10,
        'override': {
            'test_feature_flag': {'enabled': False}
        }
    }


@pytest.fixture
def sample_experiment_data():
    """Sample experiment data for testing."""
    return {
        'experiment_name': 'test_experiment',
        'context': {
            'and': [
                {'==': [{'var': 'country'}, 'US']},
                {'>=': [{'var': 'age'}, 18]}
            ]
        },
        'variants': [
            {'id': 'control', 'weight': 50},
            {'id': 'treatment', 'weight': 50}
        ],
        'overrides': {
            'control': {'feature_x': 'control_value'},
            'treatment': {'feature_x': 'treatment_value'}
        },
        'traffic_percentage': 100
    }


@pytest.fixture
def sample_experiment_group_data():
    """Sample experiment group data for testing."""
    return {
        'group_name': 'test_group',
        'description': 'Test experiment group',
        'members': ['exp1', 'exp2']
    }


@pytest.fixture
def mock_mcp_server():
    """Mock MCP server instance for testing."""
    server = MagicMock()
    server.serve = AsyncMock()
    server.shutdown = AsyncMock()
    return server


@pytest.fixture
def mock_tool_responses():
    """Mock responses for various tools."""
    return {
        'get_config': {
            'status': 'success',
            'data': {
                'configs': {'test_key': 'test_value'}
            }
        },
        'set_config': {
            'status': 'success',
            'message': 'Configuration updated successfully'
        },
        'delete_config': {
            'status': 'success',
            'message': 'Configuration deleted successfully'
        },
        'list_configs': {
            'status': 'success',
            'data': {
                'configs': [
                    {'key': 'config1', 'value': 'value1'},
                    {'key': 'config2', 'value': 'value2'}
                ]
            }
        },
        'create_experiment': {
            'status': 'success',
            'data': {
                'experiment_id': 'exp_123',
                'name': 'test_experiment'
            }
        },
        'get_experiment': {
            'status': 'success',
            'data': {
                'experiment_id': 'exp_123',
                'name': 'test_experiment',
                'status': 'RUNNING'
            }
        }
    }


@pytest.fixture
def mock_error_responses():
    """Mock error responses for testing error scenarios."""
    return {
        'network_error': {
            'error': 'NetworkError',
            'message': 'Connection timeout',
            'code': 'NETWORK_TIMEOUT'
        },
        'auth_error': {
            'error': 'AuthenticationError',
            'message': 'Invalid authentication token',
            'code': 'AUTH_FAILED'
        },
        'validation_error': {
            'error': 'ValidationError',
            'message': 'Invalid input parameters',
            'code': 'VALIDATION_FAILED',
            'details': ['Missing required field: tenant']
        },
        'server_error': {
            'error': 'ServerError',
            'message': 'Internal server error',
            'code': 'INTERNAL_ERROR'
        }
    }


@pytest.fixture
def mock_sdk_models():
    """Mock SDK model classes for testing."""
    
    class MockGetConfigInput:
        def __init__(self, tenant: str, key: str, context: Optional[Dict] = None):
            self.tenant = tenant
            self.key = key
            self.context = context or {}
    
    class MockGetConfigOutput:
        def __init__(self, value: Any):
            self.value = value
    
    class MockCreateDefaultConfigInput:
        def __init__(self, tenant: str, key: str, value: Any, schema: Optional[Dict] = None):
            self.tenant = tenant
            self.key = key
            self.value = value
            self.schema = schema
    
    class MockCreateContextInput:
        def __init__(self, tenant: str, context_id: str, condition: Dict, override: Dict, priority: int = 1):
            self.tenant = tenant
            self.context_id = context_id
            self.condition = condition
            self.override = override
            self.priority = priority
    
    class MockCreateExperimentInput:
        def __init__(self, tenant: str, experiment_name: str, context: Dict, variants: List[Dict], 
                     overrides: Dict, traffic_percentage: int = 100):
            self.tenant = tenant
            self.experiment_name = experiment_name
            self.context = context
            self.variants = variants
            self.overrides = overrides
            self.traffic_percentage = traffic_percentage
    
    return {
        'GetConfigInput': MockGetConfigInput,
        'GetConfigOutput': MockGetConfigOutput,
        'CreateDefaultConfigInput': MockCreateDefaultConfigInput,
        'CreateContextInput': MockCreateContextInput,
        'CreateExperimentInput': MockCreateExperimentInput
    }


@pytest.fixture
def mock_tool_registry():
    """Mock tool registry for MCP server testing."""
    registry = {}
    
    def register_tool(name: str, handler):
        registry[name] = handler
    
    def get_tool(name: str):
        return registry.get(name)
    
    def list_tools():
        return list(registry.keys())
    
    return {
        'register': register_tool,
        'get': get_tool,
        'list': list_tools,
        'registry': registry
    }


# Test data factories
class TestDataFactory:
    """Factory for creating test data objects."""
    
    @staticmethod
    def create_config_request(tenant: str = "test", key: str = "test_key", 
                            context: Optional[Dict] = None) -> Dict[str, Any]:
        return {
            'tenant': tenant,
            'key': key,
            'context': context or {}
        }
    
    @staticmethod
    def create_experiment_request(tenant: str = "test", name: str = "test_exp",
                                variants: Optional[List[Dict]] = None) -> Dict[str, Any]:
        return {
            'tenant': tenant,
            'experiment_name': name,
            'context': {'==': [{'var': 'user_type'}, 'premium']},
            'variants': variants or [
                {'id': 'control', 'weight': 50},
                {'id': 'treatment', 'weight': 50}
            ],
            'overrides': {
                'control': {'feature': 'off'},
                'treatment': {'feature': 'on'}
            }
        }


@pytest.fixture
def test_data_factory():
    """Provide test data factory."""
    return TestDataFactory


# Environment setup
@pytest.fixture(autouse=True)
def setup_test_environment():
    """Set up test environment variables."""
    test_env = {
        'SUPERPOSITION_ENDPOINT': 'http://localhost:8080',
        'SUPERPOSITION_TENANT': 'test',
        'SUPERPOSITION_AUTH_TOKEN': 'test-token',
        'MCP_SERVER_PORT': '8081',
        'LOG_LEVEL': 'DEBUG'
    }
    
    with patch.dict(os.environ, test_env):
        yield


# Async test utilities
@pytest.fixture
def async_test_utils():
    """Utilities for async testing."""
    
    class AsyncTestUtils:
        @staticmethod
        async def call_with_timeout(coro, timeout: float = 5.0):
            """Call an async function with timeout."""
            return await asyncio.wait_for(coro, timeout=timeout)
        
        @staticmethod
        async def assert_raises_async(exception_class, coro):
            """Assert that an async function raises a specific exception."""
            with pytest.raises(exception_class):
                await coro
    
    return AsyncTestUtils()