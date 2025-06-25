import asyncio
import logging

from .provider import SuperpositionProvider
from .types import SuperpositionProviderOptions, PollingStrategy
from openfeature import api
from openfeature.evaluation_context import EvaluationContext

# Set up logging for debugging
logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)


def check_boolean_val(client, ctx):
    """Test boolean flag evaluation with assertions - matches Java testBooleanEvaluation"""
    bool_result = client.get_boolean_details("bool", True, ctx)  # Using "bool" key like Java test
    print(f"Boolean flag 'bool': {bool_result.value} (reason: {bool_result.reason})")
    
    # Assertions for boolean evaluation
    assert bool_result is not None, "Boolean result should not be None"
    assert hasattr(bool_result, 'value'), "Boolean result should have a value attribute"
    assert isinstance(bool_result.value, bool), f"Expected boolean, got {type(bool_result.value)}"
    assert hasattr(bool_result, 'reason'), "Boolean result should have a reason attribute"
    
    # Assert exact value like Java test expects
    assert bool_result.value == False, f"Expected boolean value to be True, but got {bool_result.value}"
    
    return bool_result
    
def check_string_val(client, ctx):
    """Test string flag evaluation with assertions - matches Java testStringEvaluation"""
    string_result = client.get_string_details("string", "", ctx)  # Using "string" key like Java test
    print(f"String flag 'string': '{string_result.value}'")
    
    # Assertions for string evaluation
    assert string_result is not None, "String result should not be None"
    assert hasattr(string_result, 'value'), "String result should have a value attribute"
    assert isinstance(string_result.value, str), f"Expected string, got {type(string_result.value)}"
    assert len(string_result.value) >= 0, "String value should have valid length"
    assert hasattr(string_result, 'reason'), "String result should have a reason attribute"
    
    # Assert exact value like Java test expects
    assert string_result.value == "something", f"Expected string value to be 'something', but got '{string_result.value}'"
    
    return string_result

def check_integer_val(client, ctx):
    """Test integer flag evaluation with assertions - matches Java testIntegerEvaluation"""
    int_result = client.get_integer_details("integer", 2, ctx)  # Using "integer" key like Java test
    print(f"Integer flag 'integer': {int_result.value}")
    
    # Assertions for integer evaluation
    assert int_result is not None, "Integer result should not be None"
    assert hasattr(int_result, 'value'), "Integer result should have a value attribute"
    assert isinstance(int_result.value, int), f"Expected integer, got {type(int_result.value)}"
    assert hasattr(int_result, 'reason'), "Integer result should have a reason attribute"
    
    # Assert exact value like Java test expects
    assert int_result.value == 1, f"Expected integer value to be 1, but got {int_result.value}"
    
    return int_result

def check_float_val(client, ctx):
    """Test float flag evaluation with assertions - matches Java testDoubleEvaluation"""
    float_result = client.get_float_details("double", 2.0, ctx)  # Using "double" key like Java test
    print(f"Float flag 'double': {float_result.value}")
    
    # Assertions for float evaluation
    assert float_result is not None, "Float result should not be None"
    assert hasattr(float_result, 'value'), "Float result should have a value attribute"
    assert isinstance(float_result.value, (int, float)), f"Expected number, got {type(float_result.value)}"
    assert hasattr(float_result, 'reason'), "Float result should have a reason attribute"
    
    # Assert exact value like Java test expects
    assert float_result.value == 1.2, f"Expected float value to be 1.2, but got {float_result.value}"
    
    return float_result

def check_object_val(client, ctx):
    """Test object flag evaluation with assertions - matches Java testValueNestedObjectEvaluation"""
    obj_result = client.get_object_details("object", {}, ctx)  # Using "object" key like Java test
    print(f"Object flag 'object': {obj_result.value}")
    
    # Assertions for object evaluation
    assert obj_result is not None, "Object result should not be None"
    assert hasattr(obj_result, 'value'), "Object result should have a value attribute"
    assert obj_result.value is not None, "Object value should not be None"
    assert hasattr(obj_result, 'reason'), "Object result should have a reason attribute"
    
    # Assert exact value like Java test expects: Map.of("k1", Map.of("k2", "v1"))
    expected_object = {"k1": {"k2": "v1"}}
    assert obj_result.value == expected_object, f"Expected object value to be {expected_object}, but got {obj_result.value}"
    
    return obj_result

def check_list_val(client, ctx):
    """Test list flag evaluation with assertions - matches Java testValueNestedListEvaluation"""
    list_result = client.get_object_details("list", [], ctx)  # Using "list" key like Java test
    print(f"List flag 'list': {list_result.value}")
    
    # Assertions for list evaluation
    assert list_result is not None, "List result should not be None"
    assert hasattr(list_result, 'value'), "List result should have a value attribute"
    assert list_result.value is not None, "List value should not be None"
    assert hasattr(list_result, 'reason'), "List result should have a reason attribute"
    
    # Assert exact value like Java test expects: List.of(Value.objectToValue(Map.of("k1", "v1")))
    expected_list = [{"k1": "v1"}]
    assert list_result.value == expected_list, f"Expected list value to be {expected_list}, but got {list_result.value}"
    
    return list_result

def check_provider_status(provider):
    """Test provider status and metadata with assertions"""
    # Test provider metadata
    metadata = provider.get_metadata()
    print(f"Provider metadata: {metadata.name}")
    
    # Assertions for metadata
    assert metadata is not None, "Provider metadata should not be None"
    assert hasattr(metadata, 'name'), "Metadata should have a name attribute"
    assert isinstance(metadata.name, str), "Metadata name should be a string"
    assert len(metadata.name) > 0, "Metadata name should not be empty"
    assert metadata.name == "SuperpositionProvider", f"Expected provider name to be 'SuperpositionProvider', but got '{metadata.name}'"
    
    # Test provider status
    status = provider.get_status()
    print(f"Provider status: {status}")
    
    # Assertions for status
    assert status is not None, "Provider status should not be None"
    assert hasattr(status, 'name'), "Status should have a name attribute"
    assert status.name in ['NOT_READY', 'READY', 'STALE', 'ERROR', 'FATAL'], f"Invalid status: {status.name}"
    assert status.name == 'READY', f"Expected provider status to be 'READY', but got '{status.name}'"
    
    return metadata, status

async def test_config():
    """Integration test function - enhanced version of original test"""
    print("Testing SuperpositionProvider integration...")

    config_options = SuperpositionProviderOptions(
        endpoint="http://localhost:8080",
        token="token",
        org_id="localorg", 
        workspace_id="test",
        refresh_strategy=PollingStrategy(
            interval=5,  # Poll every 5 seconds
            timeout=3    # Timeout after 3 seconds
        ),
        fallback_config=None,
        evaluation_cache_options=None,
        experimentation_options=None
    )
    
    provider = SuperpositionProvider(provider_options=config_options)
    ctx = EvaluationContext(
        targeting_key="test-user",
        attributes={'d1': 'd1'}
    )
    
    try:
        # Initialize provider
        await provider.initialize(context=ctx)
        api.set_provider(provider)
        client = api.get_client()

        # Test boolean flag
        bool_result = check_boolean_val(client, ctx)
        
        # Test string flag
        string_result = check_string_val(client, ctx)
        
        # Test integer flag
        int_result = check_integer_val(client, ctx)
        
        # Test object flag
        obj_result = check_object_val(client, ctx)
        
        # Test float flag
        float_result = check_float_val(client, ctx)
        
        # Check provider status and metadata
        metadata, status = check_provider_status(provider)

        # Wait and test again to verify polling
        print("Waiting 15 seconds to test polling refresh...")
        await asyncio.sleep(15)

        # Test again after polling interval
        bool_result_2 = client.get_boolean_details("bool_key", True, ctx)
        print(f"Boolean flag 'bool_key' (after refresh): {bool_result_2.value}")
        
    except Exception as e:
        print(f"Test failed with error: {e}")
        logger.exception("Test execution failed")
    finally:
        # Cleanup
        if hasattr(provider, 'shutdown'):
            await provider.shutdown()
        print("Test cleanup completed")


if __name__ == "__main__":
    # Run unit tests
    print("Running integration test...")
    asyncio.run(test_config())


