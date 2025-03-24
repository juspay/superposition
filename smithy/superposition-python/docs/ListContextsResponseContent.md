# ListContextsResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**total_pages** | **float** |  | [optional] 
**total_items** | **float** |  | [optional] 
**data** | [**List[ContextFull]**](ContextFull.md) |  | [optional] 

## Example

```python
from superposition_sdk_python.models.list_contexts_response_content import ListContextsResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of ListContextsResponseContent from a JSON string
list_contexts_response_content_instance = ListContextsResponseContent.from_json(json)
# print the JSON string representation of the object
print(ListContextsResponseContent.to_json())

# convert the object into a dict
list_contexts_response_content_dict = list_contexts_response_content_instance.to_dict()
# create an instance of ListContextsResponseContent from a dict
list_contexts_response_content_from_dict = ListContextsResponseContent.from_dict(list_contexts_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


