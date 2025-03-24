# MoveContextRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**context** | **Dict[str, object]** |  | 
**description** | **str** |  | [optional] 
**change_reason** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.move_context_request_content import MoveContextRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of MoveContextRequestContent from a JSON string
move_context_request_content_instance = MoveContextRequestContent.from_json(json)
# print the JSON string representation of the object
print(MoveContextRequestContent.to_json())

# convert the object into a dict
move_context_request_content_dict = move_context_request_content_instance.to_dict()
# create an instance of MoveContextRequestContent from a dict
move_context_request_content_from_dict = MoveContextRequestContent.from_dict(move_context_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


