# UpdateOverrideRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**context** | **Dict[str, object]** |  | 
**override** | **Dict[str, object]** |  | 
**description** | **str** |  | [optional] 
**change_reason** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.update_override_request_content import UpdateOverrideRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of UpdateOverrideRequestContent from a JSON string
update_override_request_content_instance = UpdateOverrideRequestContent.from_json(json)
# print the JSON string representation of the object
print(UpdateOverrideRequestContent.to_json())

# convert the object into a dict
update_override_request_content_dict = update_override_request_content_instance.to_dict()
# create an instance of UpdateOverrideRequestContent from a dict
update_override_request_content_from_dict = UpdateOverrideRequestContent.from_dict(update_override_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


