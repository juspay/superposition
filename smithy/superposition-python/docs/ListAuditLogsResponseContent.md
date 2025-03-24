# ListAuditLogsResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**total_pages** | **float** |  | [optional] 
**total_items** | **float** |  | [optional] 
**data** | [**List[AuditLogFull]**](AuditLogFull.md) |  | [optional] 

## Example

```python
from superposition_sdk_python.models.list_audit_logs_response_content import ListAuditLogsResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of ListAuditLogsResponseContent from a JSON string
list_audit_logs_response_content_instance = ListAuditLogsResponseContent.from_json(json)
# print the JSON string representation of the object
print(ListAuditLogsResponseContent.to_json())

# convert the object into a dict
list_audit_logs_response_content_dict = list_audit_logs_response_content_instance.to_dict()
# create an instance of ListAuditLogsResponseContent from a dict
list_audit_logs_response_content_from_dict = ListAuditLogsResponseContent.from_dict(list_audit_logs_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


