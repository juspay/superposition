# ListOrganisationResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**total_pages** | **float** |  | 
**total_items** | **float** |  | 
**data** | [**List[OrganisationResponse]**](OrganisationResponse.md) |  | 

## Example

```python
from superposition_sdk_python.models.list_organisation_response_content import ListOrganisationResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of ListOrganisationResponseContent from a JSON string
list_organisation_response_content_instance = ListOrganisationResponseContent.from_json(json)
# print the JSON string representation of the object
print(ListOrganisationResponseContent.to_json())

# convert the object into a dict
list_organisation_response_content_dict = list_organisation_response_content_instance.to_dict()
# create an instance of ListOrganisationResponseContent from a dict
list_organisation_response_content_from_dict = ListOrganisationResponseContent.from_dict(list_organisation_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


