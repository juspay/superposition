# UpdateTypeTemplatesRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**type_schema** | **object** |  | 
**description** | **str** |  | [optional] 
**change_reason** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.update_type_templates_request_content import UpdateTypeTemplatesRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of UpdateTypeTemplatesRequestContent from a JSON string
update_type_templates_request_content_instance = UpdateTypeTemplatesRequestContent.from_json(json)
# print the JSON string representation of the object
print(UpdateTypeTemplatesRequestContent.to_json())

# convert the object into a dict
update_type_templates_request_content_dict = update_type_templates_request_content_instance.to_dict()
# create an instance of UpdateTypeTemplatesRequestContent from a dict
update_type_templates_request_content_from_dict = UpdateTypeTemplatesRequestContent.from_dict(update_type_templates_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


