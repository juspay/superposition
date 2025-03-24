# GetTypeTemplatesListResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**total_pages** | **float** |  | 
**total_items** | **float** |  | 
**data** | [**List[TypeTemplatesResponse]**](TypeTemplatesResponse.md) |  | 

## Example

```python
from superposition_sdk_python.models.get_type_templates_list_response_content import GetTypeTemplatesListResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of GetTypeTemplatesListResponseContent from a JSON string
get_type_templates_list_response_content_instance = GetTypeTemplatesListResponseContent.from_json(json)
# print the JSON string representation of the object
print(GetTypeTemplatesListResponseContent.to_json())

# convert the object into a dict
get_type_templates_list_response_content_dict = get_type_templates_list_response_content_instance.to_dict()
# create an instance of GetTypeTemplatesListResponseContent from a dict
get_type_templates_list_response_content_from_dict = GetTypeTemplatesListResponseContent.from_dict(get_type_templates_list_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


