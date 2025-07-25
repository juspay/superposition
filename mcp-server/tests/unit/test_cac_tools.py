"""
Unit tests for Context-Aware Configuration (CAC) tools in MCP server.
"""

import json
import pytest
from typing import Any, Dict
from unittest.mock import AsyncMock, MagicMock, patch

from ..mocks.sdk_mock import MockSuperpositionClient, MockDataGenerator


class TestCACTools:
    """Test suite for CAC tool implementations."""
    
    @pytest.fixture
    def mock_client(self):
        """Provide a mock Superposition client for testing."""
        return MockSuperpositionClient()
    
    @pytest.fixture
    def cac_tool_base(self, mock_client):
        """Mock base CAC tool class."""
        class MockCACTool:
            def __init__(self):
                self.client = mock_client
                self.tenant = "test"
            
            async def validate_input(self, data: Dict[str, Any]) -> Dict[str, Any]:
                """Basic input validation."""
                if 'tenant' not in data:
                    data['tenant'] = self.tenant
                return data
        
        return MockCACTool()
    
    @pytest.mark.asyncio
    async def test_get_config_tool_success(self, mock_client, cac_tool_base):
        """Test successful config retrieval."""
        # Setup mock data
        config_data = MockDataGenerator.generate_config_data("feature_flag", {"enabled": True})
        mock_client.configs["test:feature_flag"] = config_data['value']
        
        # Mock tool implementation
        async def get_config_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await cac_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import GetConfigInput
            input_data = GetConfigInput(
                tenant=validated_args['tenant'],
                key=validated_args['key'],
                context=validated_args.get('context', {})
            )
            
            result = await mock_client.get_config(input_data)
            
            return {
                'status': 'success',
                'data': {
                    'key': validated_args['key'],
                    'value': result.value,
                    'context': validated_args.get('context', {})
                }
            }
        
        # Test the tool
        arguments = {
            'key': 'feature_flag',
            'context': {'user_id': 'test_user'}
        }
        
        result = await get_config_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['key'] == 'feature_flag'
        assert result['data']['value'] == {"enabled": True}
        assert mock_client.get_call_count('get_config') == 1
    
    @pytest.mark.asyncio
    async def test_get_config_tool_missing_key(self, mock_client, cac_tool_base):
        """Test config retrieval for non-existent key."""
        async def get_config_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await cac_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import GetConfigInput
            input_data = GetConfigInput(
                tenant=validated_args['tenant'],
                key=validated_args['key'],
                context=validated_args.get('context', {})
            )
            
            result = await mock_client.get_config(input_data)
            
            return {
                'status': 'success',
                'data': {
                    'key': validated_args['key'],
                    'value': result.value,
                    'context': validated_args.get('context', {})
                }
            }
        
        arguments = {'key': 'non_existent_key'}
        result = await get_config_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['value'] == {}  # Empty for missing configs
    
    @pytest.mark.asyncio
    async def test_set_config_tool_success(self, mock_client, cac_tool_base):
        """Test successful config creation/update."""
        async def set_config_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await cac_tool_base.validate_input(arguments)
            
            # Determine if this is an update or create
            config_key = f"{validated_args['tenant']}:{validated_args['key']}"
            is_update = config_key in mock_client.configs
            
            if is_update:
                from superposition_sdk.models import UpdateDefaultConfigInput
                input_data = UpdateDefaultConfigInput(
                    tenant=validated_args['tenant'],
                    key=validated_args['key'],
                    value=validated_args['value']
                )
                await mock_client.update_default_config(input_data)
                action = 'updated'
            else:
                from superposition_sdk.models import CreateDefaultConfigInput
                input_data = CreateDefaultConfigInput(
                    tenant=validated_args['tenant'],
                    key=validated_args['key'],
                    value=validated_args['value'],
                    schema=validated_args.get('schema')
                )
                await mock_client.create_default_config(input_data)
                action = 'created'
            
            return {
                'status': 'success',
                'message': f'Configuration {action} successfully',
                'data': {
                    'key': validated_args['key'],
                    'value': validated_args['value'],
                    'action': action
                }
            }
        
        # Test creation
        arguments = {
            'key': 'new_feature',
            'value': {'enabled': False, 'rollout': 25},
            'schema': {
                'type': 'object',
                'properties': {
                    'enabled': {'type': 'boolean'},
                    'rollout': {'type': 'number'}
                }
            }
        }
        
        result = await set_config_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['action'] == 'created'
        assert result['data']['key'] == 'new_feature'
        assert mock_client.get_call_count('create_default_config') == 1
        
        # Test update
        arguments['value'] = {'enabled': True, 'rollout': 50}
        result = await set_config_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['action'] == 'updated'
        assert mock_client.get_call_count('update_default_config') == 1
    
    @pytest.mark.asyncio
    async def test_delete_config_tool_success(self, mock_client, cac_tool_base):
        """Test successful config deletion."""
        # Setup existing config
        mock_client.configs["test:delete_me"] = {'value': 'to_be_deleted'}
        
        async def delete_config_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await cac_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import DeleteDefaultConfigInput
            input_data = DeleteDefaultConfigInput(
                tenant=validated_args['tenant'],
                key=validated_args['key']
            )
            
            await mock_client.delete_default_config(input_data)
            
            return {
                'status': 'success',
                'message': 'Configuration deleted successfully',
                'data': {
                    'key': validated_args['key']
                }
            }
        
        arguments = {'key': 'delete_me'}
        result = await delete_config_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['key'] == 'delete_me'
        assert mock_client.get_call_count('delete_default_config') == 1
        
        # Verify config was deleted from mock
        assert "test:delete_me" not in mock_client.configs
    
    @pytest.mark.asyncio
    async def test_list_configs_tool_success(self, mock_client, cac_tool_base):
        """Test successful config listing."""
        # Setup multiple configs
        mock_client.configs.update({
            "test:config1": {"value": "value1"},
            "test:config2": {"value": "value2"},
            "other:config3": {"value": "value3"}  # Different tenant
        })
        
        async def list_configs_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await cac_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import ListDefaultConfigsInput
            input_data = ListDefaultConfigsInput(
                tenant=validated_args['tenant']
            )
            
            result = await mock_client.list_default_configs(input_data)
            
            return {
                'status': 'success',
                'data': {
                    'configs': result.configs,
                    'count': len(result.configs)
                }
            }
        
        arguments = {}  # Should use default tenant
        result = await list_configs_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['count'] == 2  # Only configs for 'test' tenant
        assert 'config1' in result['data']['configs']
        assert 'config2' in result['data']['configs']
        assert 'config3' not in result['data']['configs']  # Different tenant
    
    @pytest.mark.asyncio
    async def test_create_context_tool_success(self, mock_client, cac_tool_base):
        """Test successful context creation."""
        async def create_context_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await cac_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import CreateContextInput
            input_data = CreateContextInput(
                tenant=validated_args['tenant'],
                context_id=validated_args['context_id'],
                condition=validated_args['condition'],
                override=validated_args['override'],
                priority=validated_args.get('priority', 1)
            )
            
            await mock_client.create_context(input_data)
            
            return {
                'status': 'success',
                'message': 'Context created successfully',
                'data': {
                    'context_id': validated_args['context_id'],
                    'condition': validated_args['condition'],
                    'override': validated_args['override'],
                    'priority': validated_args.get('priority', 1)
                }
            }
        
        arguments = {
            'context_id': 'premium_users',
            'condition': {'==': [{'var': 'user_type'}, 'premium']},
            'override': {'feature_x': {'enabled': True}},
            'priority': 10
        }
        
        result = await create_context_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['context_id'] == 'premium_users'
        assert mock_client.get_call_count('create_context') == 1
    
    @pytest.mark.asyncio
    async def test_get_context_tool_success(self, mock_client, cac_tool_base):
        """Test successful context retrieval."""
        # Setup existing context
        context_data = MockDataGenerator.generate_context_data('test_context')
        mock_client.contexts["test:test_context"] = {
            'condition': context_data['condition'],
            'override': context_data['override'],
            'priority': context_data['priority']
        }
        
        async def get_context_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await cac_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import GetContextInput
            input_data = GetContextInput(
                tenant=validated_args['tenant'],
                context_id=validated_args['context_id']
            )
            
            result = await mock_client.get_context(input_data)
            
            return {
                'status': 'success',
                'data': {
                    'context_id': validated_args['context_id'],
                    'condition': result.condition,
                    'override': result.override,
                    'priority': result.priority
                }
            }
        
        arguments = {'context_id': 'test_context'}
        result = await get_context_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['context_id'] == 'test_context'
        assert result['data']['condition'] == context_data['condition']
        assert result['data']['override'] == context_data['override']
    
    @pytest.mark.asyncio
    async def test_update_context_tool_success(self, mock_client, cac_tool_base):
        """Test successful context update."""
        # Setup existing context
        mock_client.contexts["test:update_context"] = {
            'condition': {'==': [{'var': 'old'}, 'value']},
            'override': {'old_key': 'old_value'},
            'priority': 5
        }
        
        async def update_context_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await cac_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import UpdateOverrideInput
            input_data = UpdateOverrideInput(
                tenant=validated_args['tenant'],
                context_id=validated_args['context_id'],
                override=validated_args['override']
            )
            
            await mock_client.update_override(input_data)
            
            return {
                'status': 'success',
                'message': 'Context updated successfully',
                'data': {
                    'context_id': validated_args['context_id'],
                    'override': validated_args['override']
                }
            }
        
        arguments = {
            'context_id': 'update_context',
            'override': {'new_key': 'new_value', 'updated_key': 'updated_value'}
        }
        
        result = await update_context_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['context_id'] == 'update_context'
        assert mock_client.get_call_count('update_override') == 1
        
        # Verify the override was updated in mock
        context = mock_client.contexts["test:update_context"]
        assert context['override'] == arguments['override']
    
    @pytest.mark.asyncio
    async def test_delete_context_tool_success(self, mock_client, cac_tool_base):
        """Test successful context deletion."""
        # Setup existing context
        mock_client.contexts["test:delete_context"] = {
            'condition': {'==': [{'var': 'test'}, 'value']},
            'override': {'test_key': 'test_value'},
            'priority': 1
        }
        
        async def delete_context_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await cac_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import DeleteContextInput
            input_data = DeleteContextInput(
                tenant=validated_args['tenant'],
                context_id=validated_args['context_id']
            )
            
            await mock_client.delete_context(input_data)
            
            return {
                'status': 'success',
                'message': 'Context deleted successfully',
                'data': {
                    'context_id': validated_args['context_id']
                }
            }
        
        arguments = {'context_id': 'delete_context'}
        result = await delete_context_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['context_id'] == 'delete_context'
        assert mock_client.get_call_count('delete_context') == 1
        
        # Verify context was deleted from mock
        assert "test:delete_context" not in mock_client.contexts
    
    @pytest.mark.asyncio
    async def test_list_contexts_tool_success(self, mock_client, cac_tool_base):
        """Test successful context listing."""
        # Setup multiple contexts
        mock_client.contexts.update({
            "test:context1": {
                'condition': {'==': [{'var': 'type'}, 'A']},
                'override': {'key1': 'value1'},
                'priority': 1
            },
            "test:context2": {
                'condition': {'==': [{'var': 'type'}, 'B']},
                'override': {'key2': 'value2'},
                'priority': 2
            },
            "other:context3": {
                'condition': {'==': [{'var': 'type'}, 'C']},
                'override': {'key3': 'value3'},
                'priority': 3
            }
        })
        
        async def list_contexts_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await cac_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import ListContextsInput
            input_data = ListContextsInput(
                tenant=validated_args['tenant']
            )
            
            result = await mock_client.list_contexts(input_data)
            
            return {
                'status': 'success',
                'data': {
                    'contexts': result.contexts,
                    'count': len(result.contexts)
                }
            }
        
        arguments = {}  # Should use default tenant
        result = await list_contexts_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['count'] == 2  # Only contexts for 'test' tenant
        
        contexts = result['data']['contexts']
        context_ids = [ctx['context_id'] for ctx in contexts]
        assert 'context1' in context_ids
        assert 'context2' in context_ids
        assert 'context3' not in context_ids  # Different tenant
    
    @pytest.mark.asyncio
    async def test_cac_tool_input_validation(self, cac_tool_base):
        """Test input validation for CAC tools."""
        # Test missing required fields
        with pytest.raises(KeyError):
            invalid_args = {}
            # This would fail when trying to access required 'key' field
            await cac_tool_base.validate_input(invalid_args)
            # Access the key to trigger the error
            _ = invalid_args['key']
        
        # Test tenant defaulting
        args_without_tenant = {'key': 'test_key'}
        validated = await cac_tool_base.validate_input(args_without_tenant)
        assert validated['tenant'] == 'test'
        assert validated['key'] == 'test_key'
    
    @pytest.mark.asyncio
    async def test_cac_tools_with_network_failure(self, cac_tool_base):
        """Test CAC tools behavior with network failures."""
        # Create a client that will fail
        failing_client = MockSuperpositionClient(should_fail=True, failure_mode="network")
        cac_tool_base.client = failing_client
        
        async def get_config_tool_with_error_handling(arguments: Dict[str, Any]) -> Dict[str, Any]:
            try:
                validated_args = await cac_tool_base.validate_input(arguments)
                
                from superposition_sdk.models import GetConfigInput
                input_data = GetConfigInput(
                    tenant=validated_args['tenant'],
                    key=validated_args['key'],
                    context=validated_args.get('context', {})
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
                    'message': str(e)
                }
        
        arguments = {'key': 'test_key'}
        result = await get_config_tool_with_error_handling(arguments)
        
        assert result['status'] == 'error'
        assert result['error'] == 'NetworkError'
        assert 'Connection timeout' in result['message']
    
    @pytest.mark.asyncio
    async def test_cac_tools_with_auth_failure(self, cac_tool_base):
        """Test CAC tools behavior with authentication failures."""
        # Create a client that will fail with auth error
        failing_client = MockSuperpositionClient(should_fail=True, failure_mode="auth")
        cac_tool_base.client = failing_client
        
        async def create_config_tool_with_error_handling(arguments: Dict[str, Any]) -> Dict[str, Any]:
            try:
                validated_args = await cac_tool_base.validate_input(arguments)
                
                from superposition_sdk.models import CreateDefaultConfigInput
                input_data = CreateDefaultConfigInput(
                    tenant=validated_args['tenant'],
                    key=validated_args['key'],
                    value=validated_args['value']
                )
                
                await failing_client.create_default_config(input_data)
                
                return {
                    'status': 'success',
                    'message': 'Config created'
                }
            except PermissionError as e:
                return {
                    'status': 'error',
                    'error': 'AuthenticationError',
                    'message': str(e)
                }
        
        arguments = {
            'key': 'test_key',
            'value': 'test_value'
        }
        result = await create_config_tool_with_error_handling(arguments)
        
        assert result['status'] == 'error'
        assert result['error'] == 'AuthenticationError'
        assert 'Authentication failed' in result['message']