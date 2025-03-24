# CreateDimensionRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**dimension** | **str** |  | 
**position** | **float** |  | 
**var_schema** | **object** |  | 
**function_name** | **str** |  | [optional] 
**description** | **str** |  | 
**change_reason** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.create_dimension_request_content import CreateDimensionRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of CreateDimensionRequestContent from a JSON string
create_dimension_request_content_instance = CreateDimensionRequestContent.from_json(json)
# print the JSON string representation of the object
print(CreateDimensionRequestContent.to_json())

# convert the object into a dict
create_dimension_request_content_dict = create_dimension_request_content_instance.to_dict()
# create an instance of CreateDimensionRequestContent from a dict
create_dimension_request_content_from_dict = CreateDimensionRequestContent.from_dict(create_dimension_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


