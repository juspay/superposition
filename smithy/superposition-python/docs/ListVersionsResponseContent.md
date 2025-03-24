# ListVersionsResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**total_pages** | **float** |  | [optional] 
**total_items** | **float** |  | [optional] 
**data** | **List[str]** |  | [optional] 

## Example

```python
from superposition_sdk_python.models.list_versions_response_content import ListVersionsResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of ListVersionsResponseContent from a JSON string
list_versions_response_content_instance = ListVersionsResponseContent.from_json(json)
# print the JSON string representation of the object
print(ListVersionsResponseContent.to_json())

# convert the object into a dict
list_versions_response_content_dict = list_versions_response_content_instance.to_dict()
# create an instance of ListVersionsResponseContent from a dict
list_versions_response_content_from_dict = ListVersionsResponseContent.from_dict(list_versions_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


