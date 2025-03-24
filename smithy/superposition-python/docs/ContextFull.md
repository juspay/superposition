# ContextFull


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **str** |  | 
**value** | **Dict[str, object]** |  | [optional] 
**override** | **Dict[str, object]** |  | [optional] 
**override_id** | **str** |  | [optional] 
**weight** | **float** |  | [optional] 
**override_with_keys** | **List[str]** |  | [optional] 
**description** | **str** |  | [optional] 
**change_reason** | **str** |  | [optional] 
**created_at** | **datetime** |  | [optional] 
**created_by** | **str** |  | [optional] 
**last_modified_at** | **datetime** |  | [optional] 
**last_modified_by** | **str** |  | [optional] 

## Example

```python
from superposition_sdk_python.models.context_full import ContextFull

# TODO update the JSON string below
json = "{}"
# create an instance of ContextFull from a JSON string
context_full_instance = ContextFull.from_json(json)
# print the JSON string representation of the object
print(ContextFull.to_json())

# convert the object into a dict
context_full_dict = context_full_instance.to_dict()
# create an instance of ContextFull from a dict
context_full_from_dict = ContextFull.from_dict(context_full_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


