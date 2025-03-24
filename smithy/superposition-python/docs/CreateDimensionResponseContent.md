# CreateDimensionResponseContent


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
from superposition_sdk_python.models.create_dimension_response_content import CreateDimensionResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of CreateDimensionResponseContent from a JSON string
create_dimension_response_content_instance = CreateDimensionResponseContent.from_json(json)
# print the JSON string representation of the object
print(CreateDimensionResponseContent.to_json())

# convert the object into a dict
create_dimension_response_content_dict = create_dimension_response_content_instance.to_dict()
# create an instance of CreateDimensionResponseContent from a dict
create_dimension_response_content_from_dict = CreateDimensionResponseContent.from_dict(create_dimension_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


