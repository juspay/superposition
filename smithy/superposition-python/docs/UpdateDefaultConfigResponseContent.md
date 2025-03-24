# UpdateDefaultConfigResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**key** | **str** |  | 
**value** | **object** |  | 
**var_schema** | **object** |  | 
**description** | **str** |  | 
**change_reason** | **str** |  | 
**function_name** | **str** | Optional | [optional] 
**created_at** | **datetime** |  | 
**created_by** | **str** |  | 
**last_modified_at** | **datetime** |  | 
**last_modified_by** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.update_default_config_response_content import UpdateDefaultConfigResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of UpdateDefaultConfigResponseContent from a JSON string
update_default_config_response_content_instance = UpdateDefaultConfigResponseContent.from_json(json)
# print the JSON string representation of the object
print(UpdateDefaultConfigResponseContent.to_json())

# convert the object into a dict
update_default_config_response_content_dict = update_default_config_response_content_instance.to_dict()
# create an instance of UpdateDefaultConfigResponseContent from a dict
update_default_config_response_content_from_dict = UpdateDefaultConfigResponseContent.from_dict(update_default_config_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


