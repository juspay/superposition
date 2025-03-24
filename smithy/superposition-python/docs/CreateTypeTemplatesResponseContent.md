# CreateTypeTemplatesResponseContent


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
from superposition_sdk_python.models.create_type_templates_response_content import CreateTypeTemplatesResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of CreateTypeTemplatesResponseContent from a JSON string
create_type_templates_response_content_instance = CreateTypeTemplatesResponseContent.from_json(json)
# print the JSON string representation of the object
print(CreateTypeTemplatesResponseContent.to_json())

# convert the object into a dict
create_type_templates_response_content_dict = create_type_templates_response_content_instance.to_dict()
# create an instance of CreateTypeTemplatesResponseContent from a dict
create_type_templates_response_content_from_dict = CreateTypeTemplatesResponseContent.from_dict(create_type_templates_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


