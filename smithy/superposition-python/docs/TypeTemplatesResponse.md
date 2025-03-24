# TypeTemplatesResponse


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
from superposition_sdk_python.models.type_templates_response import TypeTemplatesResponse

# TODO update the JSON string below
json = "{}"
# create an instance of TypeTemplatesResponse from a JSON string
type_templates_response_instance = TypeTemplatesResponse.from_json(json)
# print the JSON string representation of the object
print(TypeTemplatesResponse.to_json())

# convert the object into a dict
type_templates_response_dict = type_templates_response_instance.to_dict()
# create an instance of TypeTemplatesResponse from a dict
type_templates_response_from_dict = TypeTemplatesResponse.from_dict(type_templates_response_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


