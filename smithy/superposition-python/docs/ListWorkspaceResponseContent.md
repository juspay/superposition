# ListWorkspaceResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**total_pages** | **float** |  | 
**total_items** | **float** |  | 
**data** | [**List[WorkspaceResponse]**](WorkspaceResponse.md) |  | 

## Example

```python
from superposition_sdk_python.models.list_workspace_response_content import ListWorkspaceResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of ListWorkspaceResponseContent from a JSON string
list_workspace_response_content_instance = ListWorkspaceResponseContent.from_json(json)
# print the JSON string representation of the object
print(ListWorkspaceResponseContent.to_json())

# convert the object into a dict
list_workspace_response_content_dict = list_workspace_response_content_instance.to_dict()
# create an instance of ListWorkspaceResponseContent from a dict
list_workspace_response_content_from_dict = ListWorkspaceResponseContent.from_dict(list_workspace_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


