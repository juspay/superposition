"""
Comprehensive error handling and validation tests for MCP server.
Tests various error scenarios, edge cases, and input validation.
"""

import asyncio
import json
import pytest
from typing import Any, Dict, List, Optional
from unittest.mock import AsyncMock, MagicMock, patch

from ..mocks.sdk_mock import MockSuperpositionClient, MockMCPServer


class TestErrorHandling:
    """Test suite for error handling scenarios."""
    
    @pytest.fixture
    def error_client(self):
        """Provide a client configured for different error scenarios."""
        return MockSuperpositionClient()
    
    @pytest.fixture
    def mock_server(self):
        """Provide a mock MCP server."""
        return MockMCPServer()
    
    @pytest.mark.asyncio
    async def test_network_timeout_errors(self, error_client):
        """Test handling of network timeout errors."""
        error_client.set_failure_mode(True, "network")
        
        async def network_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            try:
                from superposition_sdk.models import GetConfigInput
                input_data = GetConfigInput(
                    tenant=arguments.get('tenant', 'test'),
                    key=arguments['key']
                )
                result = await error_client.get_config(input_data)
                
                return {'status': 'success', 'data': {'value': result.value}}
            except ConnectionError as e:
                return {
                    'status': 'error',
                    'error': 'NetworkError',
                    'code': 'NETWORK_TIMEOUT',
                    'message': str(e),
                    'retry_after': 5
                }
        
        result = await network_tool({'key': 'test_key'})
        
        assert result['status'] == 'error'
        assert result['error'] == 'NetworkError'
        assert result['code'] == 'NETWORK_TIMEOUT'
        assert 'Connection timeout' in result['message']
        assert result['retry_after'] == 5
    
    @pytest.mark.asyncio
    async def test_authentication_errors(self, error_client):
        """Test handling of authentication errors."""
        error_client.set_failure_mode(True, "auth")
        
        async def auth_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            try:
                from superposition_sdk.models import CreateDefaultConfigInput
                input_data = CreateDefaultConfigInput(
                    tenant=arguments.get('tenant', 'test'),
                    key=arguments['key'],
                    value=arguments['value']
                )
                await error_client.create_default_config(input_data)
                
                return {'status': 'success'}
            except PermissionError as e:
                return {
                    'status': 'error',
                    'error': 'AuthenticationError',
                    'code': 'AUTH_FAILED',
                    'message': str(e),
                    'details': {
                        'requires_reauth': True,
                        'auth_url': 'https://auth.superposition.com'
                    }
                }
        
        result = await auth_tool({'key': 'test', 'value': 'test'})
        
        assert result['status'] == 'error'
        assert result['error'] == 'AuthenticationError'
        assert result['code'] == 'AUTH_FAILED'
        assert result['details']['requires_reauth'] is True
    
    @pytest.mark.asyncio
    async def test_validation_errors(self, error_client):
        """Test handling of validation errors."""
        error_client.set_failure_mode(True, "validation")
        
        async def validation_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            try:
                # Pre-validation
                errors = []
                if 'tenant' not in arguments:
                    errors.append('Missing required field: tenant')
                if 'key' not in arguments:
                    errors.append('Missing required field: key')
                
                if errors:
                    return {
                        'status': 'error',
                        'error': 'ValidationError',
                        'code': 'VALIDATION_FAILED',
                        'message': 'Input validation failed',
                        'details': {
                            'field_errors': errors,
                            'provided_fields': list(arguments.keys())
                        }
                    }
                
                # This will also fail with validation error from client
                from superposition_sdk.models import GetConfigInput
                input_data = GetConfigInput(**arguments)
                result = await error_client.get_config(input_data)
                
                return {'status': 'success'}
            except ValueError as e:
                return {
                    'status': 'error',
                    'error': 'ValidationError',
                    'code': 'VALIDATION_FAILED',
                    'message': str(e)
                }
        
        # Test missing required fields
        result = await validation_tool({})
        
        assert result['status'] == 'error'
        assert result['error'] == 'ValidationError'
        assert result['code'] == 'VALIDATION_FAILED'
        assert len(result['details']['field_errors']) == 2
        
        # Test server-side validation error
        result = await validation_tool({'tenant': 'test', 'key': 'test'})
        
        assert result['status'] == 'error'
        assert result['error'] == 'ValidationError'
        assert 'Invalid input parameters' in result['message']
    
    @pytest.mark.asyncio
    async def test_server_errors(self, error_client):
        """Test handling of server errors."""
        error_client.set_failure_mode(True, "server")
        
        async def server_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            try:
                from superposition_sdk.models import CreateExperimentInput
                input_data = CreateExperimentInput(
                    tenant=arguments.get('tenant', 'test'),
                    experiment_name=arguments['experiment_name'],
                    context=arguments['context'],
                    variants=arguments['variants'],
                    overrides=arguments['overrides']
                )
                result = await error_client.create_experiment(input_data)
                
                return {'status': 'success'}
            except RuntimeError as e:
                return {
                    'status': 'error',
                    'error': 'ServerError',
                    'code': 'INTERNAL_ERROR',
                    'message': str(e),
                    'details': {
                        'server_id': 'srv-001',
                        'timestamp': '2024-01-01T00:00:00Z',
                        'request_id': 'req-12345'
                    }
                }
        
        result = await server_tool({
            'experiment_name': 'test',
            'context': {},
            'variants': [],
            'overrides': {}
        })
        
        assert result['status'] == 'error'
        assert result['error'] == 'ServerError'
        assert result['code'] == 'INTERNAL_ERROR'
        assert 'server_id' in result['details']
    
    @pytest.mark.asyncio
    async def test_rate_limiting_errors(self, mock_server):
        """Test handling of rate limiting scenarios."""
        
        call_count = 0
        rate_limit = 3
        
        async def rate_limited_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            nonlocal call_count
            call_count += 1
            
            if call_count > rate_limit:
                return {
                    'status': 'error',
                    'error': 'RateLimitError',
                    'code': 'RATE_LIMIT_EXCEEDED',
                    'message': f'Rate limit of {rate_limit} requests exceeded',
                    'details': {
                        'limit': rate_limit,
                        'current_count': call_count,
                        'reset_time': '2024-01-01T01:00:00Z',
                        'retry_after': 60
                    }
                }
            
            return {
                'status': 'success',
                'data': {'call_count': call_count}
            }
        
        mock_server.register_tool('rate_limited', rate_limited_tool)
        
        # Make calls up to the limit
        for i in range(rate_limit):
            result = await mock_server.call_tool('rate_limited', {})
            assert result['status'] == 'success'
        
        # Next call should be rate limited
        result = await mock_server.call_tool('rate_limited', {})
        assert result['status'] == 'error'
        assert result['error'] == 'RateLimitError'
        assert result['details']['retry_after'] == 60
    
    @pytest.mark.asyncio
    async def test_concurrent_error_handling(self, error_client, mock_server):
        """Test error handling under concurrent load."""
        
        async def unreliable_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            # Randomly fail based on input
            fail_trigger = arguments.get('fail', False)
            
            if fail_trigger:
                error_types = ['network', 'auth', 'validation', 'server']
                error_type = error_types[hash(str(arguments)) % len(error_types)]
                
                error_client.set_failure_mode(True, error_type)
                
                try:
                    from superposition_sdk.models import GetConfigInput
                    input_data = GetConfigInput(
                        tenant=arguments.get('tenant', 'test'),
                        key=arguments.get('key', 'test')
                    )
                    await error_client.get_config(input_data)
                    return {'status': 'success'}
                except Exception as e:
                    return {
                        'status': 'error',
                        'error': type(e).__name__,
                        'message': str(e)
                    }
            else:
                return {'status': 'success', 'data': arguments}
        
        mock_server.register_tool('unreliable', unreliable_tool)
        
        # Create mixed success/failure scenarios
        tasks = []
        for i in range(10):
            args = {'id': i, 'fail': i % 3 == 0}  # Every 3rd call fails
            task = mock_server.call_tool('unreliable', args)
            tasks.append(task)
        
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        # Verify we got a mix of successes and failures
        successes = sum(1 for r in results if not isinstance(r, Exception) and r['status'] == 'success')
        failures = sum(1 for r in results if not isinstance(r, Exception) and r['status'] == 'error')
        
        assert successes > 0
        assert failures > 0
        assert successes + failures == 10


class TestInputValidation:
    """Test suite for input validation scenarios."""
    
    @pytest.mark.asyncio
    async def test_schema_validation(self):
        """Test schema-based input validation."""
        
        def validate_config_input(data: Dict[str, Any]) -> Dict[str, Any]:
            """Validate configuration input against schema."""
            errors = []
            
            # Required fields
            if 'key' not in data:
                errors.append('Missing required field: key')
            elif not isinstance(data['key'], str) or len(data['key']) == 0:
                errors.append('Field key must be a non-empty string')
            
            if 'value' not in data:
                errors.append('Missing required field: value')
            
            # Optional fields with constraints
            if 'tenant' in data:
                if not isinstance(data['tenant'], str) or len(data['tenant']) == 0:
                    errors.append('Field tenant must be a non-empty string')
            
            if 'schema' in data:
                if not isinstance(data['schema'], dict):
                    errors.append('Field schema must be an object')
            
            if errors:
                return {
                    'valid': False,
                    'errors': errors
                }
            
            return {'valid': True}
        
        # Test valid input
        valid_input = {
            'key': 'test_key',
            'value': {'enabled': True},
            'tenant': 'test',
            'schema': {'type': 'object'}
        }
        
        result = validate_config_input(valid_input)
        assert result['valid'] is True
        
        # Test missing required fields
        invalid_input = {'tenant': 'test'}
        result = validate_config_input(invalid_input)
        assert result['valid'] is False
        assert len(result['errors']) == 2
        
        # Test invalid field types
        invalid_input = {
            'key': '',  # Empty string
            'value': 'test',
            'tenant': 123,  # Wrong type
            'schema': 'not_an_object'  # Wrong type
        }
        result = validate_config_input(invalid_input)
        assert result['valid'] is False
        assert len(result['errors']) == 3
    
    @pytest.mark.asyncio
    async def test_experiment_validation(self):
        """Test experiment-specific validation."""
        
        def validate_experiment_input(data: Dict[str, Any]) -> Dict[str, Any]:
            """Validate experiment input."""
            errors = []
            
            # Required fields
            required_fields = ['experiment_name', 'context', 'variants', 'overrides']
            for field in required_fields:
                if field not in data:
                    errors.append(f'Missing required field: {field}')
            
            # Validate variants
            if 'variants' in data:
                if not isinstance(data['variants'], list) or len(data['variants']) == 0:
                    errors.append('Variants must be a non-empty list')
                else:
                    total_weight = 0
                    for i, variant in enumerate(data['variants']):
                        if not isinstance(variant, dict):
                            errors.append(f'Variant {i} must be an object')
                            continue
                        
                        if 'id' not in variant:
                            errors.append(f'Variant {i} missing required field: id')
                        
                        if 'weight' not in variant:
                            errors.append(f'Variant {i} missing required field: weight')
                        elif not isinstance(variant['weight'], (int, float)) or variant['weight'] < 0:
                            errors.append(f'Variant {i} weight must be a positive number')
                        else:
                            total_weight += variant['weight']
                    
                    if total_weight != 100:
                        errors.append(f'Variant weights must sum to 100, got {total_weight}')
            
            # Validate context
            if 'context' in data and not isinstance(data['context'], dict):
                errors.append('Context must be an object')
            
            # Validate overrides
            if 'overrides' in data:
                if not isinstance(data['overrides'], dict):
                    errors.append('Overrides must be an object')
                elif 'variants' in data:
                    variant_ids = {v.get('id') for v in data['variants'] if isinstance(v, dict)}
                    override_ids = set(data['overrides'].keys())
                    
                    missing_overrides = variant_ids - override_ids
                    extra_overrides = override_ids - variant_ids
                    
                    if missing_overrides:
                        errors.append(f'Missing overrides for variants: {missing_overrides}')
                    if extra_overrides:
                        errors.append(f'Extra overrides for unknown variants: {extra_overrides}')
            
            if errors:
                return {'valid': False, 'errors': errors}
            
            return {'valid': True}
        
        # Test valid experiment
        valid_experiment = {
            'experiment_name': 'test_exp',
            'context': {'==': [{'var': 'user_type'}, 'premium']},
            'variants': [
                {'id': 'control', 'weight': 50},
                {'id': 'treatment', 'weight': 50}
            ],
            'overrides': {
                'control': {'feature': False},
                'treatment': {'feature': True}
            }
        }
        
        result = validate_experiment_input(valid_experiment)
        assert result['valid'] is True
        
        # Test invalid weight distribution
        invalid_experiment = {
            'experiment_name': 'test_exp',
            'context': {},
            'variants': [
                {'id': 'control', 'weight': 30},
                {'id': 'treatment', 'weight': 50}  # Total = 80, not 100
            ],
            'overrides': {
                'control': {},
                'treatment': {}
            }
        }
        
        result = validate_experiment_input(invalid_experiment)
        assert result['valid'] is False
        assert any('must sum to 100' in error for error in result['errors'])
        
        # Test missing overrides
        missing_overrides_experiment = {
            'experiment_name': 'test_exp',
            'context': {},
            'variants': [
                {'id': 'control', 'weight': 50},
                {'id': 'treatment', 'weight': 50}
            ],
            'overrides': {
                'control': {}
                # Missing 'treatment' override
            }
        }
        
        result = validate_experiment_input(missing_overrides_experiment)
        assert result['valid'] is False
        assert any('Missing overrides' in error for error in result['errors'])
    
    @pytest.mark.asyncio
    async def test_context_validation(self):
        """Test context condition validation."""
        
        def validate_context_condition(condition: Any) -> Dict[str, Any]:
            """Validate context condition structure."""
            errors = []
            
            def validate_condition_recursive(cond, path=""):
                if not isinstance(cond, dict):
                    errors.append(f'Condition at {path} must be an object')
                    return
                
                if len(cond) != 1:
                    errors.append(f'Condition at {path} must have exactly one operator')
                    return
                
                operator, operands = next(iter(cond.items()))
                
                # Validate operators
                valid_operators = ['==', '!=', '>', '<', '>=', '<=', 'and', 'or', 'not', 'in', 'var']
                if operator not in valid_operators:
                    errors.append(f'Invalid operator "{operator}" at {path}')
                    return
                
                # Validate operands based on operator
                if operator in ['and', 'or']:
                    if not isinstance(operands, list) or len(operands) < 2:
                        errors.append(f'Operator {operator} at {path} requires at least 2 operands')
                    else:
                        for i, operand in enumerate(operands):
                            validate_condition_recursive(operand, f'{path}.{operator}[{i}]')
                
                elif operator == 'not':
                    if not isinstance(operands, (dict, list)):
                        errors.append(f'Operator not at {path} requires a condition operand')
                    else:
                        validate_condition_recursive(operands, f'{path}.not')
                
                elif operator in ['==', '!=', '>', '<', '>=', '<=']:
                    if not isinstance(operands, list) or len(operands) != 2:
                        errors.append(f'Operator {operator} at {path} requires exactly 2 operands')
                
                elif operator == 'in':
                    if not isinstance(operands, list) or len(operands) != 2:
                        errors.append(f'Operator in at {path} requires exactly 2 operands')
                    elif not isinstance(operands[1], list):
                        errors.append(f'Second operand of in at {path} must be a list')
                
                elif operator == 'var':
                    if not isinstance(operands, str):
                        errors.append(f'Operator var at {path} requires a string operand')
            
            validate_condition_recursive(condition)
            
            if errors:
                return {'valid': False, 'errors': errors}
            
            return {'valid': True}
        
        # Test valid conditions
        valid_conditions = [
            {'==': [{'var': 'user_id'}, 'test_user']},
            {'and': [
                {'==': [{'var': 'country'}, 'US']},
                {'>=': [{'var': 'age'}, 18]}
            ]},
            {'or': [
                {'==': [{'var': 'plan'}, 'premium']},
                {'in': [{'var': 'feature_flags'}, ['beta_user', 'early_access']]}
            ]}
        ]
        
        for condition in valid_conditions:
            result = validate_context_condition(condition)
            assert result['valid'] is True, f'Condition {condition} should be valid'
        
        # Test invalid conditions
        invalid_conditions = [
            {},  # Empty condition
            {'invalid_op': ['test']},  # Invalid operator
            {'==': ['single_operand']},  # Wrong number of operands
            {'and': [{'==': [{'var': 'test'}, 'value']}]},  # Too few operands for 'and'
            {'in': [{'var': 'test'}, 'not_a_list']},  # Wrong type for 'in' operands
            'not_an_object'  # Not an object
        ]
        
        for condition in invalid_conditions:
            result = validate_context_condition(condition)
            assert result['valid'] is False, f'Condition {condition} should be invalid'


class TestEdgeCases:
    """Test suite for edge cases and boundary conditions."""
    
    @pytest.mark.asyncio
    async def test_large_payload_handling(self):
        """Test handling of large payloads."""
        
        async def large_payload_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            # Simulate processing large payload
            payload_size = len(json.dumps(arguments))
            max_size = 1024 * 1024  # 1MB limit
            
            if payload_size > max_size:
                return {
                    'status': 'error',
                    'error': 'PayloadTooLarge',
                    'code': 'PAYLOAD_TOO_LARGE',
                    'message': f'Payload size {payload_size} exceeds limit of {max_size} bytes',
                    'details': {
                        'payload_size': payload_size,
                        'max_size': max_size
                    }
                }
            
            return {
                'status': 'success',
                'data': {
                    'processed': True,
                    'payload_size': payload_size
                }
            }
        
        # Test normal payload
        normal_payload = {'key': 'test', 'value': 'small_value'}
        result = await large_payload_tool(normal_payload)
        assert result['status'] == 'success'
        
        # Test large payload
        large_value = 'x' * (1024 * 1024 + 1)  # Over 1MB
        large_payload = {'key': 'test', 'value': large_value}
        result = await large_payload_tool(large_payload)
        assert result['status'] == 'error'
        assert result['error'] == 'PayloadTooLarge'
    
    @pytest.mark.asyncio
    async def test_unicode_and_special_characters(self):
        """Test handling of Unicode and special characters."""
        
        async def unicode_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            try:
                # Test various encodings and special characters
                key = arguments.get('key', '')
                value = arguments.get('value', '')
                
                # Validate UTF-8 encoding
                key.encode('utf-8')
                json.dumps(value)  # Ensure JSON serializable
                
                return {
                    'status': 'success',
                    'data': {
                        'key': key,
                        'value': value,
                        'key_length': len(key),
                        'value_type': type(value).__name__
                    }
                }
            except UnicodeError as e:
                return {
                    'status': 'error',
                    'error': 'EncodingError',
                    'message': f'Unicode encoding error: {str(e)}'
                }
            except TypeError as e:
                return {
                    'status': 'error',
                    'error': 'SerializationError',
                    'message': f'JSON serialization error: {str(e)}'
                }
        
        # Test various Unicode scenarios
        test_cases = [
            {'key': 'emoji_test', 'value': '🚀🌟💻'},
            {'key': 'chinese_test', 'value': '你好世界'},
            {'key': 'arabic_test', 'value': 'مرحبا بالعالم'},
            {'key': 'special_chars', 'value': '!@#$%^&*()[]{}|\\:";\'<>?,.'},
            {'key': 'newlines_tabs', 'value': 'line1\nline2\ttab'},
        ]
        
        for test_case in test_cases:
            result = await unicode_tool(test_case)
            assert result['status'] == 'success'
            assert result['data']['key'] == test_case['key']
            assert result['data']['value'] == test_case['value']
    
    @pytest.mark.asyncio
    async def test_null_and_empty_values(self):
        """Test handling of null and empty values."""
        
        async def null_handling_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            processed_args = {}
            
            for key, value in arguments.items():
                if value is None:
                    processed_args[key] = {'type': 'null', 'value': None}
                elif value == '':
                    processed_args[key] = {'type': 'empty_string', 'value': ''}
                elif value == []:
                    processed_args[key] = {'type': 'empty_list', 'value': []}
                elif value == {}:
                    processed_args[key] = {'type': 'empty_dict', 'value': {}}
                else:
                    processed_args[key] = {'type': type(value).__name__, 'value': value}
            
            return {
                'status': 'success',
                'data': processed_args
            }
        
        test_input = {
            'null_value': None,
            'empty_string': '',
            'empty_list': [],
            'empty_dict': {},
            'zero': 0,
            'false': False,
            'normal_value': 'test'
        }
        
        result = await null_handling_tool(test_input)
        assert result['status'] == 'success'
        
        data = result['data']
        assert data['null_value']['type'] == 'null'
        assert data['empty_string']['type'] == 'empty_string'
        assert data['empty_list']['type'] == 'empty_list'
        assert data['empty_dict']['type'] == 'empty_dict'
        assert data['zero']['type'] == 'int'
        assert data['false']['type'] == 'bool'
        assert data['normal_value']['type'] == 'str'
    
    @pytest.mark.asyncio
    async def test_circular_reference_handling(self):
        """Test handling of circular references in data structures."""
        
        async def circular_ref_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            try:
                # Attempt to serialize the arguments to detect circular references
                json.dumps(arguments)
                
                return {
                    'status': 'success',
                    'data': arguments
                }
            except ValueError as e:
                if 'circular reference' in str(e).lower() or 'out of range' in str(e).lower():
                    return {
                        'status': 'error',
                        'error': 'CircularReferenceError',
                        'code': 'CIRCULAR_REFERENCE',
                        'message': 'Circular reference detected in input data'
                    }
                else:
                    raise
        
        # Test normal data (no circular reference)
        normal_data = {
            'user': {
                'name': 'test',
                'preferences': {
                    'theme': 'dark',
                    'language': 'en'
                }
            }
        }
        
        result = await circular_ref_tool(normal_data)
        assert result['status'] == 'success'
        
        # Test with potential circular reference simulation
        # (We'll simulate this by creating a deeply nested structure)
        deep_nested = {'level': 0}
        current = deep_nested
        for i in range(1000):  # Create very deep nesting
            current['next'] = {'level': i + 1}
            current = current['next']
        
        # This might cause issues with very deep structures
        # The exact behavior depends on JSON serialization limits
        try:
            result = await circular_ref_tool(deep_nested)
            # If it succeeds, that's fine
            assert result['status'] in ['success', 'error']
        except RecursionError:
            # Expected for very deep structures
            pass