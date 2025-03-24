# UpdateDimensionRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**var_schema** | **object** |  | [optional] 
**function_name** | **str** |  | [optional] 
**description** | **str** |  | [optional] 
**change_reason** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.update_dimension_request_content import UpdateDimensionRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of UpdateDimensionRequestContent from a JSON string
update_dimension_request_content_instance = UpdateDimensionRequestContent.from_json(json)
# print the JSON string representation of the object
print(UpdateDimensionRequestContent.to_json())

# convert the object into a dict
update_dimension_request_content_dict = update_dimension_request_content_instance.to_dict()
# create an instance of UpdateDimensionRequestContent from a dict
update_dimension_request_content_from_dict = UpdateDimensionRequestContent.from_dict(update_dimension_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


