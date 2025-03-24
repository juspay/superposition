# CreateContextRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**context** | **Dict[str, object]** |  | 
**override** | **Dict[str, object]** |  | 
**description** | **str** |  | [optional] 
**change_reason** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.create_context_request_content import CreateContextRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of CreateContextRequestContent from a JSON string
create_context_request_content_instance = CreateContextRequestContent.from_json(json)
# print the JSON string representation of the object
print(CreateContextRequestContent.to_json())

# convert the object into a dict
create_context_request_content_dict = create_context_request_content_instance.to_dict()
# create an instance of CreateContextRequestContent from a dict
create_context_request_content_from_dict = CreateContextRequestContent.from_dict(create_context_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


