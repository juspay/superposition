# UpdateTypeTemplatesResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**type_name** | **str** |  | 
**type_schema** | **object** |  | 
**description** | **str** |  | 
**change_reason** | **str** |  | 
**created_by** | **str** |  | 
**created_at** | **datetime** |  | 
**last_modified_at** | **datetime** |  | 
**last_modified_by** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.update_type_templates_response_content import UpdateTypeTemplatesResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of UpdateTypeTemplatesResponseContent from a JSON string
update_type_templates_response_content_instance = UpdateTypeTemplatesResponseContent.from_json(json)
# print the JSON string representation of the object
print(UpdateTypeTemplatesResponseContent.to_json())

# convert the object into a dict
update_type_templates_response_content_dict = update_type_templates_response_content_instance.to_dict()
# create an instance of UpdateTypeTemplatesResponseContent from a dict
update_type_templates_response_content_from_dict = UpdateTypeTemplatesResponseContent.from_dict(update_type_templates_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


