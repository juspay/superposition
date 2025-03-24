# ListDimensionsResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**total_pages** | **float** |  | [optional] 
**total_items** | **float** |  | [optional] 
**data** | [**List[DimensionExt]**](DimensionExt.md) |  | [optional] 

## Example

```python
from superposition_sdk_python.models.list_dimensions_response_content import ListDimensionsResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of ListDimensionsResponseContent from a JSON string
list_dimensions_response_content_instance = ListDimensionsResponseContent.from_json(json)
# print the JSON string representation of the object
print(ListDimensionsResponseContent.to_json())

# convert the object into a dict
list_dimensions_response_content_dict = list_dimensions_response_content_instance.to_dict()
# create an instance of ListDimensionsResponseContent from a dict
list_dimensions_response_content_from_dict = ListDimensionsResponseContent.from_dict(list_dimensions_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


