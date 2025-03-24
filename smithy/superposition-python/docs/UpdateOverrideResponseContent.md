# UpdateOverrideResponseContent


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
from superposition_sdk_python.models.update_override_response_content import UpdateOverrideResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of UpdateOverrideResponseContent from a JSON string
update_override_response_content_instance = UpdateOverrideResponseContent.from_json(json)
# print the JSON string representation of the object
print(UpdateOverrideResponseContent.to_json())

# convert the object into a dict
update_override_response_content_dict = update_override_response_content_instance.to_dict()
# create an instance of UpdateOverrideResponseContent from a dict
update_override_response_content_from_dict = UpdateOverrideResponseContent.from_dict(update_override_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


