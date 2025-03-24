# GetContextFromConditionResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **str** |  | 
**value** | **Dict[str, object]** |  | [optional] 
**override** | **Dict[str, object]** |  | [optional] 
**override_id** | **str** |  | [optional] 
**weight** | **float** |  | [optional] 
**override_with_keys** | **List[str]** |  | [optional] 
**description** | **str** |  | [optional] 
**change_reason** | **str** |  | [optional] 
**created_at** | **datetime** |  | [optional] 
**created_by** | **str** |  | [optional] 
**last_modified_at** | **datetime** |  | [optional] 
**last_modified_by** | **str** |  | [optional] 

## Example

```python
from superposition_sdk_python.models.get_context_from_condition_response_content import GetContextFromConditionResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of GetContextFromConditionResponseContent from a JSON string
get_context_from_condition_response_content_instance = GetContextFromConditionResponseContent.from_json(json)
# print the JSON string representation of the object
print(GetContextFromConditionResponseContent.to_json())

# convert the object into a dict
get_context_from_condition_response_content_dict = get_context_from_condition_response_content_instance.to_dict()
# create an instance of GetContextFromConditionResponseContent from a dict
get_context_from_condition_response_content_from_dict = GetContextFromConditionResponseContent.from_dict(get_context_from_condition_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


