# UpdateFunctionRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**description** | **str** |  | [optional] 
**change_reason** | **str** |  | 
**function** | **str** |  | 
**runtime_version** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.update_function_request_content import UpdateFunctionRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of UpdateFunctionRequestContent from a JSON string
update_function_request_content_instance = UpdateFunctionRequestContent.from_json(json)
# print the JSON string representation of the object
print(UpdateFunctionRequestContent.to_json())

# convert the object into a dict
update_function_request_content_dict = update_function_request_content_instance.to_dict()
# create an instance of UpdateFunctionRequestContent from a dict
update_function_request_content_from_dict = UpdateFunctionRequestContent.from_dict(update_function_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


