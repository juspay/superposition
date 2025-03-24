# GetContextResponseContent


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
from superposition_sdk_python.models.get_context_response_content import GetContextResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of GetContextResponseContent from a JSON string
get_context_response_content_instance = GetContextResponseContent.from_json(json)
# print the JSON string representation of the object
print(GetContextResponseContent.to_json())

# convert the object into a dict
get_context_response_content_dict = get_context_response_content_instance.to_dict()
# create an instance of GetContextResponseContent from a dict
get_context_response_content_from_dict = GetContextResponseContent.from_dict(get_context_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


