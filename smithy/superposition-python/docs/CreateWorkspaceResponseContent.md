# CreateWorkspaceResponseContent


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
from superposition_sdk_python.models.create_workspace_response_content import CreateWorkspaceResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of CreateWorkspaceResponseContent from a JSON string
create_workspace_response_content_instance = CreateWorkspaceResponseContent.from_json(json)
# print the JSON string representation of the object
print(CreateWorkspaceResponseContent.to_json())

# convert the object into a dict
create_workspace_response_content_dict = create_workspace_response_content_instance.to_dict()
# create an instance of CreateWorkspaceResponseContent from a dict
create_workspace_response_content_from_dict = CreateWorkspaceResponseContent.from_dict(create_workspace_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


