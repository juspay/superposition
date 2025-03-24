# ListDefaultConfigsResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**total_pages** | **float** |  | [optional] 
**total_items** | **float** |  | [optional] 
**data** | [**List[DefaultConfigFull]**](DefaultConfigFull.md) |  | [optional] 

## Example

```python
from superposition_sdk_python.models.list_default_configs_response_content import ListDefaultConfigsResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of ListDefaultConfigsResponseContent from a JSON string
list_default_configs_response_content_instance = ListDefaultConfigsResponseContent.from_json(json)
# print the JSON string representation of the object
print(ListDefaultConfigsResponseContent.to_json())

# convert the object into a dict
list_default_configs_response_content_dict = list_default_configs_response_content_instance.to_dict()
# create an instance of ListDefaultConfigsResponseContent from a dict
list_default_configs_response_content_from_dict = ListDefaultConfigsResponseContent.from_dict(list_default_configs_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


