# MoveContextResponseContent


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
from superposition_sdk_python.models.move_context_response_content import MoveContextResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of MoveContextResponseContent from a JSON string
move_context_response_content_instance = MoveContextResponseContent.from_json(json)
# print the JSON string representation of the object
print(MoveContextResponseContent.to_json())

# convert the object into a dict
move_context_response_content_dict = move_context_response_content_instance.to_dict()
# create an instance of MoveContextResponseContent from a dict
move_context_response_content_from_dict = MoveContextResponseContent.from_dict(move_context_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


