# UpdateWorkspaceRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**workspace_admin_email** | **str** |  | 
**mandatory_dimensions** | **List[str]** |  | [optional] 
**workspace_status** | **str** |  | [optional] 

## Example

```python
from superposition_sdk_python.models.update_workspace_request_content import UpdateWorkspaceRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of UpdateWorkspaceRequestContent from a JSON string
update_workspace_request_content_instance = UpdateWorkspaceRequestContent.from_json(json)
# print the JSON string representation of the object
print(UpdateWorkspaceRequestContent.to_json())

# convert the object into a dict
update_workspace_request_content_dict = update_workspace_request_content_instance.to_dict()
# create an instance of UpdateWorkspaceRequestContent from a dict
update_workspace_request_content_from_dict = UpdateWorkspaceRequestContent.from_dict(update_workspace_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


