# UpdateDefaultConfigRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**change_reason** | **str** |  | 
**value** | **object** |  | [optional] 
**var_schema** | **object** |  | [optional] 
**function_name** | **str** |  | [optional] 
**description** | **str** |  | [optional] 

## Example

```python
from superposition_sdk_python.models.update_default_config_request_content import UpdateDefaultConfigRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of UpdateDefaultConfigRequestContent from a JSON string
update_default_config_request_content_instance = UpdateDefaultConfigRequestContent.from_json(json)
# print the JSON string representation of the object
print(UpdateDefaultConfigRequestContent.to_json())

# convert the object into a dict
update_default_config_request_content_dict = update_default_config_request_content_instance.to_dict()
# create an instance of UpdateDefaultConfigRequestContent from a dict
update_default_config_request_content_from_dict = UpdateDefaultConfigRequestContent.from_dict(update_default_config_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


