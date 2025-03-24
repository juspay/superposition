# WorkspaceResponse


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**workspace_name** | **str** |  | 
**organisation_id** | **str** |  | 
**organisation_name** | **str** |  | 
**workspace_schema_name** | **str** |  | 
**workspace_status** | **str** |  | 
**workspace_admin_email** | **str** |  | 
**created_by** | **str** |  | 
**last_modified_by** | **str** |  | 
**last_modified_at** | **datetime** |  | 
**created_at** | **datetime** |  | 
**mandatory_dimensions** | **List[str]** |  | [optional] 

## Example

```python
from superposition_sdk_python.models.workspace_response import WorkspaceResponse

# TODO update the JSON string below
json = "{}"
# create an instance of WorkspaceResponse from a JSON string
workspace_response_instance = WorkspaceResponse.from_json(json)
# print the JSON string representation of the object
print(WorkspaceResponse.to_json())

# convert the object into a dict
workspace_response_dict = workspace_response_instance.to_dict()
# create an instance of WorkspaceResponse from a dict
workspace_response_from_dict = WorkspaceResponse.from_dict(workspace_response_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


