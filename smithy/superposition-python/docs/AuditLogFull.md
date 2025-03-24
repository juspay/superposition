# AuditLogFull


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**table_name** | **str** |  | [optional] 
**user_name** | **str** |  | [optional] 
**timestamp** | **datetime** |  | [optional] 
**action** | **str** |  | [optional] 
**original_data** | **object** |  | [optional] 
**new_data** | **object** |  | [optional] 
**query** | **str** |  | [optional] 

## Example

```python
from superposition_sdk_python.models.audit_log_full import AuditLogFull

# TODO update the JSON string below
json = "{}"
# create an instance of AuditLogFull from a JSON string
audit_log_full_instance = AuditLogFull.from_json(json)
# print the JSON string representation of the object
print(AuditLogFull.to_json())

# convert the object into a dict
audit_log_full_dict = audit_log_full_instance.to_dict()
# create an instance of AuditLogFull from a dict
audit_log_full_from_dict = AuditLogFull.from_dict(audit_log_full_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


