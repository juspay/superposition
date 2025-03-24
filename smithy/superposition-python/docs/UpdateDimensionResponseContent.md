# UpdateDimensionResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**dimension** | **str** |  | 
**position** | **float** |  | 
**var_schema** | **object** |  | 
**function_name** | **str** |  | [optional] 
**description** | **str** |  | 
**change_reason** | **str** |  | 
**last_modified_at** | **datetime** |  | 
**last_modified_by** | **str** |  | 
**created_at** | **datetime** |  | 
**created_by** | **str** |  | 
**mandatory** | **bool** |  | [optional] 

## Example

```python
from superposition_sdk_python.models.update_dimension_response_content import UpdateDimensionResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of UpdateDimensionResponseContent from a JSON string
update_dimension_response_content_instance = UpdateDimensionResponseContent.from_json(json)
# print the JSON string representation of the object
print(UpdateDimensionResponseContent.to_json())

# convert the object into a dict
update_dimension_response_content_dict = update_dimension_response_content_instance.to_dict()
# create an instance of UpdateDimensionResponseContent from a dict
update_dimension_response_content_from_dict = UpdateDimensionResponseContent.from_dict(update_dimension_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


