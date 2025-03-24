# DimensionExt


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
from superposition_sdk_python.models.dimension_ext import DimensionExt

# TODO update the JSON string below
json = "{}"
# create an instance of DimensionExt from a JSON string
dimension_ext_instance = DimensionExt.from_json(json)
# print the JSON string representation of the object
print(DimensionExt.to_json())

# convert the object into a dict
dimension_ext_dict = dimension_ext_instance.to_dict()
# create an instance of DimensionExt from a dict
dimension_ext_from_dict = DimensionExt.from_dict(dimension_ext_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


