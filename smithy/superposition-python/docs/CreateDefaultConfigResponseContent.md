# CreateDefaultConfigResponseContent


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
from superposition_sdk_python.models.create_default_config_response_content import CreateDefaultConfigResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of CreateDefaultConfigResponseContent from a JSON string
create_default_config_response_content_instance = CreateDefaultConfigResponseContent.from_json(json)
# print the JSON string representation of the object
print(CreateDefaultConfigResponseContent.to_json())

# convert the object into a dict
create_default_config_response_content_dict = create_default_config_response_content_instance.to_dict()
# create an instance of CreateDefaultConfigResponseContent from a dict
create_default_config_response_content_from_dict = CreateDefaultConfigResponseContent.from_dict(create_default_config_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


