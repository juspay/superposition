"""
Integration tests for MCP server functionality.
Tests the complete MCP server, tool registration, and protocol compliance.
"""

import asyncio
import json
import pytest
from typing import Any, Dict, List
from unittest.mock import AsyncMock, MagicMock, patch

from ..mocks.sdk_mock import MockMCPServer, MockSuperpositionClient


class TestMCPServerIntegration:
    """Test suite for MCP server integration scenarios."""
    
    @pytest.fixture
    def mock_mcp_server(self):
        """Provide a mock MCP server for testing."""
        return MockMCPServer()
    
    @pytest.fixture
    def mock_superposition_client(self):
        """Provide a mock Superposition client."""
        return MockSuperpositionClient()
    
    @pytest.fixture
    async def configured_server(self, mock_mcp_server, mock_superposition_client):
        """Provide a fully configured MCP server with all tools registered."""
        
        # Mock tool implementations that would be created by Agent 1
        async def get_config_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            """Mock get_config tool implementation."""
            tenant = arguments.get('tenant', 'test')
            key = arguments['key']
            context = arguments.get('context', {})
            
            from superposition_sdk.models import GetConfigInput
            input_data = GetConfigInput(tenant=tenant, key=key, context=context)
            result = await mock_superposition_client.get_config(input_data)
            
            return {
                'status': 'success',
                'data': {
                    'key': key,
                    'value': result.value,
                    'context': context
                }
            }
        
        async def set_config_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            """Mock set_config tool implementation."""
            tenant = arguments.get('tenant', 'test')
            key = arguments['key']
            value = arguments['value']
            schema = arguments.get('schema')
            
            from superposition_sdk.models import CreateDefaultConfigInput
            input_data = CreateDefaultConfigInput(
                tenant=tenant, key=key, value=value, schema=schema
            )
            await mock_superposition_client.create_default_config(input_data)
            
            return {
                'status': 'success',
                'message': 'Configuration created successfully',
                'data': {'key': key, 'value': value}
            }
        
        async def delete_config_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            """Mock delete_config tool implementation."""
            tenant = arguments.get('tenant', 'test')
            key = arguments['key']
            
            from superposition_sdk.models import DeleteDefaultConfigInput
            input_data = DeleteDefaultConfigInput(tenant=tenant, key=key)
            await mock_superposition_client.delete_default_config(input_data)
            
            return {
                'status': 'success',
                'message': 'Configuration deleted successfully',
                'data': {'key': key}
            }
        
        async def create_experiment_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            """Mock create_experiment tool implementation."""
            tenant = arguments.get('tenant', 'test')
            experiment_name = arguments['experiment_name']
            context = arguments['context']
            variants = arguments['variants']
            overrides = arguments['overrides']
            traffic_percentage = arguments.get('traffic_percentage', 100)
            
            from superposition_sdk.models import CreateExperimentInput
            input_data = CreateExperimentInput(
                tenant=tenant,
                experiment_name=experiment_name,
                context=context,
                variants=variants,
                overrides=overrides,
                traffic_percentage=traffic_percentage
            )
            result = await mock_superposition_client.create_experiment(input_data)
            
            return {
                'status': 'success',
                'message': 'Experiment created successfully',
                'data': {
                    'experiment_id': result.experiment_id,
                    'experiment_name': experiment_name
                }
            }
        
        # Register all tools
        mock_mcp_server.register_tool('get_config', get_config_tool)
        mock_mcp_server.register_tool('set_config', set_config_tool)
        mock_mcp_server.register_tool('delete_config', delete_config_tool)
        mock_mcp_server.register_tool('create_experiment', create_experiment_tool)
        
        # Start the server
        await mock_mcp_server.start()
        
        yield mock_mcp_server
        
        # Cleanup
        await mock_mcp_server.stop()
    
    @pytest.mark.asyncio
    async def test_server_startup_and_tool_registration(self, mock_mcp_server):
        """Test MCP server startup and tool registration."""
        
        # Test empty server initially
        assert len(mock_mcp_server.list_tools()) == 0
        assert not mock_mcp_server.is_running
        
        # Register some tools
        async def dummy_tool(args):
            return {'status': 'success'}
        
        mock_mcp_server.register_tool('test_tool', dummy_tool)
        mock_mcp_server.register_tool('another_tool', dummy_tool)
        
        # Verify registration
        assert len(mock_mcp_server.list_tools()) == 2
        assert 'test_tool' in mock_mcp_server.list_tools()
        assert 'another_tool' in mock_mcp_server.list_tools()
        
        # Start server
        await mock_mcp_server.start()
        assert mock_mcp_server.is_running
        
        # Stop server
        await mock_mcp_server.stop()
        assert not mock_mcp_server.is_running
    
    @pytest.mark.asyncio
    async def test_tool_discovery_and_listing(self, configured_server):
        """Test tool discovery and listing functionality."""
        
        # Get list of available tools
        tools = configured_server.list_tools()
        
        # Verify expected tools are registered
        expected_tools = ['get_config', 'set_config', 'delete_config', 'create_experiment']
        for tool in expected_tools:
            assert tool in tools
        
        # Verify we can get tool handlers
        for tool_name in expected_tools:
            tool_handler = configured_server.get_tool(tool_name)
            assert tool_handler is not None
            assert callable(tool_handler)
    
    @pytest.mark.asyncio
    async def test_end_to_end_config_workflow(self, configured_server, mock_superposition_client):
        """Test complete configuration management workflow."""
        
        # 1. Set a configuration
        set_result = await configured_server.call_tool('set_config', {
            'key': 'test_feature',
            'value': {'enabled': True, 'percentage': 50},
            'schema': {
                'type': 'object',
                'properties': {
                    'enabled': {'type': 'boolean'},
                    'percentage': {'type': 'number'}
                }
            }
        })
        
        assert set_result['status'] == 'success'
        assert set_result['data']['key'] == 'test_feature'
        
        # 2. Get the configuration
        get_result = await configured_server.call_tool('get_config', {
            'key': 'test_feature',
            'context': {'user_type': 'premium'}
        })
        
        assert get_result['status'] == 'success'
        assert get_result['data']['key'] == 'test_feature'
        assert get_result['data']['value'] == {'enabled': True, 'percentage': 50}
        
        # 3. Delete the configuration
        delete_result = await configured_server.call_tool('delete_config', {
            'key': 'test_feature'
        })
        
        assert delete_result['status'] == 'success'
        assert delete_result['data']['key'] == 'test_feature'
        
        # Verify SDK client was called appropriately
        assert mock_superposition_client.get_call_count('create_default_config') == 1
        assert mock_superposition_client.get_call_count('get_config') == 1
        assert mock_superposition_client.get_call_count('delete_default_config') == 1
    
    @pytest.mark.asyncio
    async def test_end_to_end_experiment_workflow(self, configured_server, mock_superposition_client):
        """Test complete experiment management workflow."""
        
        # Create an experiment
        create_result = await configured_server.call_tool('create_experiment', {
            'experiment_name': 'feature_rollout_test',
            'context': {'==': [{'var': 'region'}, 'US']},
            'variants': [
                {'id': 'control', 'weight': 50},
                {'id': 'treatment', 'weight': 50}
            ],
            'overrides': {
                'control': {'new_feature': False},
                'treatment': {'new_feature': True}
            },
            'traffic_percentage': 100
        })
        
        assert create_result['status'] == 'success'
        assert create_result['data']['experiment_name'] == 'feature_rollout_test'
        assert 'experiment_id' in create_result['data']
        
        # Verify SDK client was called
        assert mock_superposition_client.get_call_count('create_experiment') == 1
        
        last_call = mock_superposition_client.get_last_call('create_experiment')
        assert last_call is not None
        assert last_call['args']['experiment_name'] == 'feature_rollout_test'
    
    @pytest.mark.asyncio
    async def test_concurrent_tool_calls(self, configured_server, mock_superposition_client):
        """Test handling of concurrent tool calls."""
        
        # Define multiple concurrent operations
        async def config_operation_1():
            return await configured_server.call_tool('set_config', {
                'key': 'config_1',
                'value': 'value_1'
            })
        
        async def config_operation_2():
            return await configured_server.call_tool('set_config', {
                'key': 'config_2',
                'value': 'value_2'
            })
        
        async def experiment_operation():
            return await configured_server.call_tool('create_experiment', {
                'experiment_name': 'concurrent_test',
                'context': {'==': [{'var': 'test'}, 'concurrent']},
                'variants': [{'id': 'control', 'weight': 100}],
                'overrides': {'control': {'test': 'concurrent'}}
            })
        
        # Execute operations concurrently
        results = await asyncio.gather(
            config_operation_1(),
            config_operation_2(),
            experiment_operation(),
            return_exceptions=True
        )
        
        # Verify all operations succeeded
        assert len(results) == 3
        for result in results:
            assert not isinstance(result, Exception)
            assert result['status'] == 'success'
        
        # Verify all operations were recorded
        assert mock_superposition_client.get_call_count('create_default_config') == 2
        assert mock_superposition_client.get_call_count('create_experiment') == 1
    
    @pytest.mark.asyncio
    async def test_tool_error_handling(self, configured_server):
        """Test error handling for tool calls."""
        
        # Test with invalid tool name
        with pytest.raises(ValueError, match="Tool not found"):
            await configured_server.call_tool('non_existent_tool', {})
        
        # Test with missing required arguments
        try:
            result = await configured_server.call_tool('get_config', {})
            # This might succeed or fail depending on implementation
            # If it succeeds, it should handle missing args gracefully
        except KeyError:
            # Expected for missing required 'key' argument
            pass
    
    @pytest.mark.asyncio
    async def test_mcp_protocol_compliance(self, configured_server):
        """Test MCP protocol compliance."""
        
        # Test tool call structure compliance
        result = await configured_server.call_tool('get_config', {
            'key': 'test_compliance',
            'context': {'compliance_test': True}
        })
        
        # Verify response structure matches MCP expectations
        assert isinstance(result, dict)
        assert 'status' in result
        assert result['status'] in ['success', 'error']
        
        if result['status'] == 'success':
            assert 'data' in result
        else:
            assert 'error' in result
            assert 'message' in result
    
    @pytest.mark.asyncio
    async def test_server_state_management(self, mock_mcp_server, mock_superposition_client):
        """Test server state management and cleanup."""
        
        # Test server state tracking
        assert len(mock_mcp_server.call_history) == 0
        assert len(mock_mcp_server.connections) == 0
        
        # Register and call a tool
        async def stateful_tool(args):
            return {'status': 'success', 'data': args}
        
        mock_mcp_server.register_tool('stateful_tool', stateful_tool)
        
        # Make some calls
        await mock_mcp_server.call_tool('stateful_tool', {'test': 'data1'})
        await mock_mcp_server.call_tool('stateful_tool', {'test': 'data2'})
        
        # Verify call history
        assert len(mock_mcp_server.call_history) == 2
        assert mock_mcp_server.call_history[0]['arguments'] == {'test': 'data1'}
        assert mock_mcp_server.call_history[1]['arguments'] == {'test': 'data2'}
        
        # Test server reset
        mock_mcp_server.reset()
        assert len(mock_mcp_server.call_history) == 0
    
    @pytest.mark.asyncio
    async def test_tool_argument_validation(self, configured_server):
        """Test tool argument validation and sanitization."""
        
        # Test with valid arguments
        valid_result = await configured_server.call_tool('set_config', {
            'key': 'valid_config',
            'value': {'setting': 'valid'},
            'tenant': 'test_tenant'
        })
        
        assert valid_result['status'] == 'success'
        
        # Test with minimal arguments (should use defaults)
        minimal_result = await configured_server.call_tool('get_config', {
            'key': 'minimal_config'
        })
        
        # Should succeed with default tenant
        assert minimal_result['status'] == 'success'
    
    @pytest.mark.asyncio
    async def test_tool_response_formatting(self, configured_server):
        """Test consistent response formatting across tools."""
        
        # Test different tool types return consistent formats
        tools_to_test = [
            ('get_config', {'key': 'format_test'}),
            ('set_config', {'key': 'format_test', 'value': 'test_value'}),
            ('create_experiment', {
                'experiment_name': 'format_test',
                'context': {'==': [{'var': 'test'}, 'format']},
                'variants': [{'id': 'control', 'weight': 100}],
                'overrides': {'control': {'test': 'format'}}
            })
        ]
        
        for tool_name, args in tools_to_test:
            result = await configured_server.call_tool(tool_name, args)
            
            # Verify consistent response structure
            assert isinstance(result, dict)
            assert 'status' in result
            
            if result['status'] == 'success':
                # Success responses should have data or message
                assert 'data' in result or 'message' in result
            else:
                # Error responses should have error info
                assert 'error' in result
                assert 'message' in result
    
    @pytest.mark.asyncio
    async def test_server_performance_characteristics(self, configured_server):
        """Test basic performance characteristics of the server."""
        
        import time
        
        # Test response time for simple operations
        start_time = time.time()
        
        await configured_server.call_tool('get_config', {'key': 'perf_test'})
        
        response_time = time.time() - start_time
        
        # Response should be reasonably fast (< 1 second for mock operations)
        assert response_time < 1.0
        
        # Test batch operations
        start_time = time.time()
        
        # Execute multiple operations
        tasks = []
        for i in range(10):
            task = configured_server.call_tool('get_config', {'key': f'batch_test_{i}'})
            tasks.append(task)
        
        await asyncio.gather(*tasks)
        
        batch_time = time.time() - start_time
        
        # Batch operations should complete in reasonable time
        assert batch_time < 5.0
    
    @pytest.mark.asyncio
    async def test_server_graceful_shutdown(self, mock_mcp_server):
        """Test graceful server shutdown."""
        
        # Start server
        await mock_mcp_server.start()
        assert mock_mcp_server.is_running
        
        # Simulate some active connections
        mock_mcp_server.connections = ['conn1', 'conn2', 'conn3']
        
        # Stop server
        await mock_mcp_server.stop()
        
        # Verify clean shutdown
        assert not mock_mcp_server.is_running
        assert len(mock_mcp_server.connections) == 0


class TestMCPServerErrorScenarios:
    """Test error scenarios and edge cases for MCP server."""
    
    @pytest.fixture
    def failing_client(self):
        """Provide a client that simulates failures."""
        return MockSuperpositionClient(should_fail=True, failure_mode="network")
    
    @pytest.mark.asyncio
    async def test_network_failure_handling(self, mock_mcp_server, failing_client):
        """Test handling of network failures."""
        
        async def network_sensitive_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            try:
                # This will simulate a network failure
                from superposition_sdk.models import GetConfigInput
                input_data = GetConfigInput(
                    tenant=arguments.get('tenant', 'test'),
                    key=arguments['key']
                )
                result = await failing_client.get_config(input_data)
                
                return {
                    'status': 'success',
                    'data': {'value': result.value}
                }
            except ConnectionError as e:
                return {
                    'status': 'error',
                    'error': 'NetworkError',
                    'message': f'Network operation failed: {str(e)}'
                }
        
        mock_mcp_server.register_tool('network_tool', network_sensitive_tool)
        
        # Test the tool with network failure
        result = await mock_mcp_server.call_tool('network_tool', {'key': 'test'})
        
        assert result['status'] == 'error'
        assert result['error'] == 'NetworkError'
        assert 'Network operation failed' in result['message']
    
    @pytest.mark.asyncio
    async def test_authentication_failure_handling(self, mock_mcp_server):
        """Test handling of authentication failures."""
        
        auth_failing_client = MockSuperpositionClient(should_fail=True, failure_mode="auth")
        
        async def auth_sensitive_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            try:
                from superposition_sdk.models import CreateDefaultConfigInput
                input_data = CreateDefaultConfigInput(
                    tenant=arguments.get('tenant', 'test'),
                    key=arguments['key'],
                    value=arguments['value']
                )
                await auth_failing_client.create_default_config(input_data)
                
                return {
                    'status': 'success',
                    'message': 'Config created'
                }
            except PermissionError as e:
                return {
                    'status': 'error',
                    'error': 'AuthenticationError',
                    'message': f'Authentication failed: {str(e)}'
                }
        
        mock_mcp_server.register_tool('auth_tool', auth_sensitive_tool)
        
        # Test the tool with auth failure
        result = await mock_mcp_server.call_tool('auth_tool', {
            'key': 'test',
            'value': 'test_value'
        })
        
        assert result['status'] == 'error'
        assert result['error'] == 'AuthenticationError'
        assert 'Authentication failed' in result['message']
    
    @pytest.mark.asyncio
    async def test_server_error_handling(self, mock_mcp_server):
        """Test handling of server errors."""
        
        server_failing_client = MockSuperpositionClient(should_fail=True, failure_mode="server")
        
        async def server_sensitive_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            try:
                from superposition_sdk.models import CreateExperimentInput
                input_data = CreateExperimentInput(
                    tenant=arguments.get('tenant', 'test'),
                    experiment_name=arguments['experiment_name'],
                    context=arguments['context'],
                    variants=arguments['variants'],
                    overrides=arguments['overrides']
                )
                result = await server_failing_client.create_experiment(input_data)
                
                return {
                    'status': 'success',
                    'data': {'experiment_id': result.experiment_id}
                }
            except RuntimeError as e:
                return {
                    'status': 'error',
                    'error': 'ServerError',
                    'message': f'Server error occurred: {str(e)}'
                }
        
        mock_mcp_server.register_tool('server_tool', server_sensitive_tool)
        
        # Test the tool with server failure
        result = await mock_mcp_server.call_tool('server_tool', {
            'experiment_name': 'test_exp',
            'context': {'==': [{'var': 'test'}, 'value']},
            'variants': [{'id': 'control', 'weight': 100}],
            'overrides': {'control': {}}
        })
        
        assert result['status'] == 'error'
        assert result['error'] == 'ServerError'
        assert 'Server error occurred' in result['message']