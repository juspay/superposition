# CreateTypeTemplatesRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**type_name** | **str** |  | 
**type_schema** | **object** |  | 
**description** | **str** |  | 
**change_reason** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.create_type_templates_request_content import CreateTypeTemplatesRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of CreateTypeTemplatesRequestContent from a JSON string
create_type_templates_request_content_instance = CreateTypeTemplatesRequestContent.from_json(json)
# print the JSON string representation of the object
print(CreateTypeTemplatesRequestContent.to_json())

# convert the object into a dict
create_type_templates_request_content_dict = create_type_templates_request_content_instance.to_dict()
# create an instance of CreateTypeTemplatesRequestContent from a dict
create_type_templates_request_content_from_dict = CreateTypeTemplatesRequestContent.from_dict(create_type_templates_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


