# CreateContextResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**context_id** | **str** |  | 
**override_id** | **str** |  | 
**weight** | **float** |  | 
**description** | **str** |  | 
**change_reason** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.create_context_response_content import CreateContextResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of CreateContextResponseContent from a JSON string
create_context_response_content_instance = CreateContextResponseContent.from_json(json)
# print the JSON string representation of the object
print(CreateContextResponseContent.to_json())

# convert the object into a dict
create_context_response_content_dict = create_context_response_content_instance.to_dict()
# create an instance of CreateContextResponseContent from a dict
create_context_response_content_from_dict = CreateContextResponseContent.from_dict(create_context_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


