# CreateWorkspaceRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**workspace_admin_email** | **str** |  | 
**workspace_name** | **str** |  | 
**workspace_status** | **str** |  | [optional] 

## Example

```python
from superposition_sdk_python.models.create_workspace_request_content import CreateWorkspaceRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of CreateWorkspaceRequestContent from a JSON string
create_workspace_request_content_instance = CreateWorkspaceRequestContent.from_json(json)
# print the JSON string representation of the object
print(CreateWorkspaceRequestContent.to_json())

# convert the object into a dict
create_workspace_request_content_dict = create_workspace_request_content_instance.to_dict()
# create an instance of CreateWorkspaceRequestContent from a dict
create_workspace_request_content_from_dict = CreateWorkspaceRequestContent.from_dict(create_workspace_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


