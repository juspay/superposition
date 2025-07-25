"""
Unit tests for Experimentation tools in MCP server.
"""

import json
import pytest
from typing import Any, Dict, List
from unittest.mock import AsyncMock, MagicMock, patch

from ..mocks.sdk_mock import MockSuperpositionClient, MockDataGenerator


class TestExperimentationTools:
    """Test suite for Experimentation tool implementations."""
    
    @pytest.fixture
    def mock_client(self):
        """Provide a mock Superposition client for testing."""
        return MockSuperpositionClient()
    
    @pytest.fixture
    def exp_tool_base(self, mock_client):
        """Mock base Experimentation tool class."""
        class MockExperimentationTool:
            def __init__(self):
                self.client = mock_client
                self.tenant = "test"
            
            async def validate_input(self, data: Dict[str, Any]) -> Dict[str, Any]:
                """Basic input validation."""
                if 'tenant' not in data:
                    data['tenant'] = self.tenant
                return data
            
            def validate_variants(self, variants: List[Dict]) -> bool:
                """Validate variant configuration."""
                total_weight = sum(variant.get('weight', 0) for variant in variants)
                return total_weight == 100
        
        return MockExperimentationTool()
    
    @pytest.mark.asyncio
    async def test_create_experiment_tool_success(self, mock_client, exp_tool_base):
        """Test successful experiment creation."""
        async def create_experiment_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await exp_tool_base.validate_input(arguments)
            
            # Validate variants
            if not exp_tool_base.validate_variants(validated_args['variants']):
                return {
                    'status': 'error',
                    'error': 'ValidationError',
                    'message': 'Variant weights must sum to 100'
                }
            
            from superposition_sdk.models import CreateExperimentInput
            input_data = CreateExperimentInput(
                tenant=validated_args['tenant'],
                experiment_name=validated_args['experiment_name'],
                context=validated_args['context'],
                variants=validated_args['variants'],
                overrides=validated_args['overrides'],
                traffic_percentage=validated_args.get('traffic_percentage', 100)
            )
            
            result = await mock_client.create_experiment(input_data)
            
            return {
                'status': 'success',
                'message': 'Experiment created successfully',
                'data': {
                    'experiment_id': result.experiment_id,
                    'experiment_name': validated_args['experiment_name'],
                    'status': 'CREATED'
                }
            }
        
        arguments = {
            'experiment_name': 'test_ab_experiment',
            'context': {'==': [{'var': 'country'}, 'US']},
            'variants': [
                {'id': 'control', 'weight': 50},
                {'id': 'treatment', 'weight': 50}
            ],
            'overrides': {
                'control': {'feature_flag': False},
                'treatment': {'feature_flag': True}
            },
            'traffic_percentage': 100
        }
        
        result = await create_experiment_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['experiment_name'] == 'test_ab_experiment'
        assert result['data']['status'] == 'CREATED'
        assert 'experiment_id' in result['data']
        assert mock_client.get_call_count('create_experiment') == 1
    
    @pytest.mark.asyncio
    async def test_create_experiment_invalid_variants(self, mock_client, exp_tool_base):
        """Test experiment creation with invalid variant weights."""
        async def create_experiment_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await exp_tool_base.validate_input(arguments)
            
            # Validate variants
            if not exp_tool_base.validate_variants(validated_args['variants']):
                return {
                    'status': 'error',
                    'error': 'ValidationError',
                    'message': 'Variant weights must sum to 100'
                }
            
            # This won't be reached due to validation failure
            return {'status': 'success'}
        
        arguments = {
            'experiment_name': 'invalid_experiment',
            'context': {'==': [{'var': 'country'}, 'US']},
            'variants': [
                {'id': 'control', 'weight': 30},  # Total = 80, not 100
                {'id': 'treatment', 'weight': 50}
            ],
            'overrides': {
                'control': {'feature': 'off'},
                'treatment': {'feature': 'on'}
            }
        }
        
        result = await create_experiment_tool(arguments)
        
        assert result['status'] == 'error'
        assert result['error'] == 'ValidationError'
        assert 'weights must sum to 100' in result['message']
        assert mock_client.get_call_count('create_experiment') == 0
    
    @pytest.mark.asyncio
    async def test_get_experiment_tool_success(self, mock_client, exp_tool_base):
        """Test successful experiment retrieval."""
        # Setup existing experiment
        experiment_data = MockDataGenerator.generate_experiment_data('test_experiment')
        mock_client.experiments["test:test_experiment"] = {
            'experiment_id': 'exp_123',
            'experiment_name': 'test_experiment',
            'context': experiment_data['context'],
            'variants': experiment_data['variants'],
            'overrides': experiment_data['overrides'],
            'traffic_percentage': experiment_data['traffic_percentage'],
            'status': 'RUNNING'
        }
        
        async def get_experiment_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await exp_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import GetExperimentInput
            input_data = GetExperimentInput(
                tenant=validated_args['tenant'],
                experiment_id=validated_args['experiment_id']
            )
            
            result = await mock_client.get_experiment(input_data)
            
            return {
                'status': 'success',
                'data': {
                    'experiment_id': result.experiment_id,
                    'experiment_name': result.experiment_name,
                    'context': result.context,
                    'variants': result.variants,
                    'overrides': result.overrides,
                    'traffic_percentage': result.traffic_percentage,
                    'status': result.status
                }
            }
        
        arguments = {'experiment_id': 'exp_123'}
        result = await get_experiment_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['experiment_id'] == 'exp_123'
        assert result['data']['experiment_name'] == 'test_experiment'
        assert result['data']['status'] == 'RUNNING'
        assert mock_client.get_call_count('get_experiment') == 1
    
    @pytest.mark.asyncio
    async def test_get_experiment_not_found(self, mock_client, exp_tool_base):
        """Test experiment retrieval for non-existent experiment."""
        async def get_experiment_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            try:
                validated_args = await exp_tool_base.validate_input(arguments)
                
                from superposition_sdk.models import GetExperimentInput
                input_data = GetExperimentInput(
                    tenant=validated_args['tenant'],
                    experiment_id=validated_args['experiment_id']
                )
                
                result = await mock_client.get_experiment(input_data)
                
                return {
                    'status': 'success',
                    'data': {
                        'experiment_id': result.experiment_id,
                        'experiment_name': result.experiment_name,
                        'status': result.status
                    }
                }
            except ValueError as e:
                return {
                    'status': 'error',
                    'error': 'NotFoundError',
                    'message': str(e)
                }
        
        arguments = {'experiment_id': 'non_existent_exp'}
        result = await get_experiment_tool(arguments)
        
        assert result['status'] == 'error'
        assert result['error'] == 'NotFoundError'
        assert 'not found' in result['message']
    
    @pytest.mark.asyncio
    async def test_list_experiments_tool_success(self, mock_client, exp_tool_base):
        """Test successful experiment listing."""
        # Setup multiple experiments
        mock_client.experiments.update({
            "test:exp1": {
                'experiment_id': 'exp_1',
                'experiment_name': 'exp1',
                'status': 'RUNNING'
            },
            "test:exp2": {
                'experiment_id': 'exp_2',
                'experiment_name': 'exp2',
                'status': 'PAUSED'
            },
            "other:exp3": {
                'experiment_id': 'exp_3',
                'experiment_name': 'exp3',
                'status': 'CONCLUDED'
            }
        })
        
        async def list_experiments_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await exp_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import ListExperimentInput
            input_data = ListExperimentInput(
                tenant=validated_args['tenant']
            )
            
            result = await mock_client.list_experiment(input_data)
            
            return {
                'status': 'success',
                'data': {
                    'experiments': result.experiments,
                    'count': len(result.experiments)
                }
            }
        
        arguments = {}  # Should use default tenant
        result = await list_experiments_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['count'] == 2  # Only experiments for 'test' tenant
        
        experiments = result['data']['experiments']
        exp_names = [exp['experiment_name'] for exp in experiments]
        assert 'exp1' in exp_names
        assert 'exp2' in exp_names
        assert 'exp3' not in exp_names  # Different tenant
    
    @pytest.mark.asyncio
    async def test_update_experiment_tool_success(self, mock_client, exp_tool_base):
        """Test successful experiment update."""
        # Setup existing experiment
        mock_client.experiments["test:update_exp"] = {
            'experiment_id': 'exp_update',
            'experiment_name': 'update_exp',
            'overrides': {
                'control': {'old_feature': 'old_value'},
                'treatment': {'old_feature': 'old_value'}
            },
            'status': 'RUNNING'
        }
        
        async def update_experiment_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await exp_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import UpdateOverridesExperimentInput
            input_data = UpdateOverridesExperimentInput(
                tenant=validated_args['tenant'],
                experiment_id=validated_args['experiment_id'],
                overrides=validated_args['overrides']
            )
            
            await mock_client.update_overrides_experiment(input_data)
            
            return {
                'status': 'success',
                'message': 'Experiment overrides updated successfully',
                'data': {
                    'experiment_id': validated_args['experiment_id'],
                    'overrides': validated_args['overrides']
                }
            }
        
        arguments = {
            'experiment_id': 'exp_update',
            'overrides': {
                'control': {'new_feature': 'control_value'},
                'treatment': {'new_feature': 'treatment_value'}
            }
        }
        
        result = await update_experiment_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['experiment_id'] == 'exp_update'
        assert mock_client.get_call_count('update_overrides_experiment') == 1
    
    @pytest.mark.asyncio
    async def test_ramp_experiment_tool_success(self, mock_client, exp_tool_base):
        """Test successful experiment ramping."""
        # Setup existing experiment
        mock_client.experiments["test:ramp_exp"] = {
            'experiment_id': 'exp_ramp',
            'experiment_name': 'ramp_exp',
            'traffic_percentage': 50,
            'status': 'CREATED'
        }
        
        async def ramp_experiment_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await exp_tool_base.validate_input(arguments)
            
            # Validate traffic percentage
            traffic = validated_args.get('traffic_percentage', 100)
            if not 0 <= traffic <= 100:
                return {
                    'status': 'error',
                    'error': 'ValidationError',
                    'message': 'Traffic percentage must be between 0 and 100'
                }
            
            from superposition_sdk.models import RampExperimentInput
            input_data = RampExperimentInput(
                tenant=validated_args['tenant'],
                experiment_id=validated_args['experiment_id'],
                traffic_percentage=traffic
            )
            
            await mock_client.ramp_experiment(input_data)
            
            return {
                'status': 'success',
                'message': f'Experiment ramped to {traffic}% traffic',
                'data': {
                    'experiment_id': validated_args['experiment_id'],
                    'traffic_percentage': traffic,
                    'status': 'RUNNING'
                }
            }
        
        arguments = {
            'experiment_id': 'exp_ramp',
            'traffic_percentage': 75
        }
        
        result = await ramp_experiment_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['traffic_percentage'] == 75
        assert result['data']['status'] == 'RUNNING'
        assert mock_client.get_call_count('ramp_experiment') == 1
    
    @pytest.mark.asyncio
    async def test_pause_experiment_tool_success(self, mock_client, exp_tool_base):
        """Test successful experiment pausing."""
        # Setup running experiment
        mock_client.experiments["test:pause_exp"] = {
            'experiment_id': 'exp_pause',
            'experiment_name': 'pause_exp',
            'status': 'RUNNING'
        }
        
        async def pause_experiment_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await exp_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import PauseExperimentInput
            input_data = PauseExperimentInput(
                tenant=validated_args['tenant'],
                experiment_id=validated_args['experiment_id']
            )
            
            await mock_client.pause_experiment(input_data)
            
            return {
                'status': 'success',
                'message': 'Experiment paused successfully',
                'data': {
                    'experiment_id': validated_args['experiment_id'],
                    'status': 'PAUSED'
                }
            }
        
        arguments = {'experiment_id': 'exp_pause'}
        result = await pause_experiment_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['status'] == 'PAUSED'
        assert mock_client.get_call_count('pause_experiment') == 1
    
    @pytest.mark.asyncio
    async def test_resume_experiment_tool_success(self, mock_client, exp_tool_base):
        """Test successful experiment resuming."""
        # Setup paused experiment
        mock_client.experiments["test:resume_exp"] = {
            'experiment_id': 'exp_resume',
            'experiment_name': 'resume_exp',
            'status': 'PAUSED'
        }
        
        async def resume_experiment_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await exp_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import ResumeExperimentInput
            input_data = ResumeExperimentInput(
                tenant=validated_args['tenant'],
                experiment_id=validated_args['experiment_id']
            )
            
            await mock_client.resume_experiment(input_data)
            
            return {
                'status': 'success',
                'message': 'Experiment resumed successfully',
                'data': {
                    'experiment_id': validated_args['experiment_id'],
                    'status': 'RUNNING'
                }
            }
        
        arguments = {'experiment_id': 'exp_resume'}
        result = await resume_experiment_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['status'] == 'RUNNING'
        assert mock_client.get_call_count('resume_experiment') == 1
    
    @pytest.mark.asyncio
    async def test_conclude_experiment_tool_success(self, mock_client, exp_tool_base):
        """Test successful experiment conclusion."""
        # Setup running experiment
        mock_client.experiments["test:conclude_exp"] = {
            'experiment_id': 'exp_conclude',
            'experiment_name': 'conclude_exp',
            'status': 'RUNNING'
        }
        
        async def conclude_experiment_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await exp_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import ConcludeExperimentInput
            input_data = ConcludeExperimentInput(
                tenant=validated_args['tenant'],
                experiment_id=validated_args['experiment_id'],
                chosen_variant=validated_args.get('chosen_variant')
            )
            
            await mock_client.conclude_experiment(input_data)
            
            return {
                'status': 'success',
                'message': 'Experiment concluded successfully',
                'data': {
                    'experiment_id': validated_args['experiment_id'],
                    'status': 'CONCLUDED',
                    'chosen_variant': validated_args.get('chosen_variant')
                }
            }
        
        arguments = {
            'experiment_id': 'exp_conclude',
            'chosen_variant': 'treatment'
        }
        
        result = await conclude_experiment_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['status'] == 'CONCLUDED'
        assert result['data']['chosen_variant'] == 'treatment'
        assert mock_client.get_call_count('conclude_experiment') == 1
    
    @pytest.mark.asyncio
    async def test_create_experiment_group_tool_success(self, mock_client, exp_tool_base):
        """Test successful experiment group creation."""
        async def create_experiment_group_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await exp_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import CreateExperimentGroupInput
            input_data = CreateExperimentGroupInput(
                tenant=validated_args['tenant'],
                group_name=validated_args['group_name'],
                description=validated_args.get('description', ''),
                members=validated_args.get('members', [])
            )
            
            result = await mock_client.create_experiment_group(input_data)
            
            return {
                'status': 'success',
                'message': 'Experiment group created successfully',
                'data': {
                    'group_id': result.group_id,
                    'group_name': validated_args['group_name'],
                    'description': validated_args.get('description', ''),
                    'members': validated_args.get('members', [])
                }
            }
        
        arguments = {
            'group_name': 'test_group',
            'description': 'Test experiment group',
            'members': ['exp_1', 'exp_2']
        }
        
        result = await create_experiment_group_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['group_name'] == 'test_group'
        assert result['data']['description'] == 'Test experiment group'
        assert result['data']['members'] == ['exp_1', 'exp_2']
        assert 'group_id' in result['data']
        assert mock_client.get_call_count('create_experiment_group') == 1
    
    @pytest.mark.asyncio
    async def test_get_experiment_group_tool_success(self, mock_client, exp_tool_base):
        """Test successful experiment group retrieval."""
        # Setup existing group
        mock_client.experiment_groups["test:test_group"] = {
            'group_id': 'group_123',
            'group_name': 'test_group',
            'description': 'Test group',
            'members': ['exp_1', 'exp_2']
        }
        
        async def get_experiment_group_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await exp_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import GetExperimentGroupInput
            input_data = GetExperimentGroupInput(
                tenant=validated_args['tenant'],
                group_id=validated_args['group_id']
            )
            
            result = await mock_client.get_experiment_group(input_data)
            
            return {
                'status': 'success',
                'data': {
                    'group_id': result.group_id,
                    'group_name': result.group_name,
                    'description': result.description,
                    'members': result.members
                }
            }
        
        arguments = {'group_id': 'group_123'}
        result = await get_experiment_group_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['group_id'] == 'group_123'
        assert result['data']['group_name'] == 'test_group'
        assert result['data']['members'] == ['exp_1', 'exp_2']
        assert mock_client.get_call_count('get_experiment_group') == 1
    
    @pytest.mark.asyncio
    async def test_list_experiment_groups_tool_success(self, mock_client, exp_tool_base):
        """Test successful experiment group listing."""
        # Setup multiple groups
        mock_client.experiment_groups.update({
            "test:group1": {
                'group_id': 'group_1',
                'group_name': 'group1',
                'description': 'First group',
                'members': ['exp_1']
            },
            "test:group2": {
                'group_id': 'group_2',
                'group_name': 'group2',
                'description': 'Second group',
                'members': ['exp_2', 'exp_3']
            },
            "other:group3": {
                'group_id': 'group_3',
                'group_name': 'group3',
                'description': 'Other tenant group',
                'members': ['exp_4']
            }
        })
        
        async def list_experiment_groups_tool(arguments: Dict[str, Any]) -> Dict[str, Any]:
            validated_args = await exp_tool_base.validate_input(arguments)
            
            from superposition_sdk.models import ListExperimentGroupsInput
            input_data = ListExperimentGroupsInput(
                tenant=validated_args['tenant']
            )
            
            result = await mock_client.list_experiment_groups(input_data)
            
            return {
                'status': 'success',
                'data': {
                    'groups': result.groups,
                    'count': len(result.groups)
                }
            }
        
        arguments = {}  # Should use default tenant
        result = await list_experiment_groups_tool(arguments)
        
        assert result['status'] == 'success'
        assert result['data']['count'] == 2  # Only groups for 'test' tenant
        
        groups = result['data']['groups']
        group_names = [group['group_name'] for group in groups]
        assert 'group1' in group_names
        assert 'group2' in group_names
        assert 'group3' not in group_names  # Different tenant
    
    @pytest.mark.asyncio
    async def test_experiment_validation_edge_cases(self, exp_tool_base):
        """Test experiment validation edge cases."""
        
        # Test empty variants
        assert not exp_tool_base.validate_variants([])
        
        # Test single variant with 100% weight
        assert exp_tool_base.validate_variants([{'id': 'single', 'weight': 100}])
        
        # Test variants with decimal weights
        assert exp_tool_base.validate_variants([
            {'id': 'a', 'weight': 33.33},
            {'id': 'b', 'weight': 33.33},
            {'id': 'c', 'weight': 33.34}
        ])
        
        # Test variants with zero weight
        assert not exp_tool_base.validate_variants([
            {'id': 'a', 'weight': 0},
            {'id': 'b', 'weight': 100}
        ])
    
    @pytest.mark.asyncio
    async def test_experimentation_tools_with_failures(self, exp_tool_base):
        """Test experimentation tools with various failure scenarios."""
        # Test with validation failure client
        failing_client = MockSuperpositionClient(should_fail=True, failure_mode="validation")
        exp_tool_base.client = failing_client
        
        async def create_experiment_with_error_handling(arguments: Dict[str, Any]) -> Dict[str, Any]:
            try:
                validated_args = await exp_tool_base.validate_input(arguments)
                
                from superposition_sdk.models import CreateExperimentInput
                input_data = CreateExperimentInput(
                    tenant=validated_args['tenant'],
                    experiment_name=validated_args['experiment_name'],
                    context=validated_args['context'],
                    variants=validated_args['variants'],
                    overrides=validated_args['overrides']
                )
                
                result = await failing_client.create_experiment(input_data)
                
                return {
                    'status': 'success',
                    'data': {'experiment_id': result.experiment_id}
                }
            except ValueError as e:
                return {
                    'status': 'error',
                    'error': 'ValidationError',
                    'message': str(e)
                }
        
        arguments = {
            'experiment_name': 'test_exp',
            'context': {'==': [{'var': 'test'}, 'value']},
            'variants': [{'id': 'control', 'weight': 50}, {'id': 'treatment', 'weight': 50}],
            'overrides': {'control': {}, 'treatment': {}}
        }
        
        result = await create_experiment_with_error_handling(arguments)
        
        assert result['status'] == 'error'
        assert result['error'] == 'ValidationError'
        assert 'Invalid input parameters' in result['message']