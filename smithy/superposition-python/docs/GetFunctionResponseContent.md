# GetFunctionResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**function_name** | **str** |  | 
**published_code** | **str** |  | [optional] 
**draft_code** | **str** |  | 
**published_runtime_version** | **str** |  | [optional] 
**draft_runtime_version** | **str** |  | 
**published_at** | **datetime** |  | [optional] 
**draft_edited_at** | **datetime** |  | 
**published_by** | **str** |  | [optional] 
**draft_edited_by** | **str** |  | 
**last_modified_at** | **datetime** |  | 
**last_modified_by** | **str** |  | 
**change_reason** | **str** |  | 
**description** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.get_function_response_content import GetFunctionResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of GetFunctionResponseContent from a JSON string
get_function_response_content_instance = GetFunctionResponseContent.from_json(json)
# print the JSON string representation of the object
print(GetFunctionResponseContent.to_json())

# convert the object into a dict
get_function_response_content_dict = get_function_response_content_instance.to_dict()
# create an instance of GetFunctionResponseContent from a dict
get_function_response_content_from_dict = GetFunctionResponseContent.from_dict(get_function_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


