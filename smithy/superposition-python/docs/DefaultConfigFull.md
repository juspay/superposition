# DefaultConfigFull


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**key** | **str** |  | 
**value** | **object** |  | 
**var_schema** | **object** |  | 
**description** | **str** |  | 
**change_reason** | **str** |  | 
**function_name** | **str** | Optional | [optional] 
**created_at** | **datetime** |  | 
**created_by** | **str** |  | 
**last_modified_at** | **datetime** |  | 
**last_modified_by** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.default_config_full import DefaultConfigFull

# TODO update the JSON string below
json = "{}"
# create an instance of DefaultConfigFull from a JSON string
default_config_full_instance = DefaultConfigFull.from_json(json)
# print the JSON string representation of the object
print(DefaultConfigFull.to_json())

# convert the object into a dict
default_config_full_dict = default_config_full_instance.to_dict()
# create an instance of DefaultConfigFull from a dict
default_config_full_from_dict = DefaultConfigFull.from_dict(default_config_full_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


