# CreateFunctionRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**function_name** | **str** |  | 
**description** | **str** |  | 
**change_reason** | **str** |  | 
**function** | **str** |  | 
**runtime_version** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.create_function_request_content import CreateFunctionRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of CreateFunctionRequestContent from a JSON string
create_function_request_content_instance = CreateFunctionRequestContent.from_json(json)
# print the JSON string representation of the object
print(CreateFunctionRequestContent.to_json())

# convert the object into a dict
create_function_request_content_dict = create_function_request_content_instance.to_dict()
# create an instance of CreateFunctionRequestContent from a dict
create_function_request_content_from_dict = CreateFunctionRequestContent.from_dict(create_function_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


