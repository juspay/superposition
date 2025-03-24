# GetOrganisationResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **str** |  | 
**name** | **str** |  | 
**country_code** | **str** |  | [optional] 
**contact_email** | **str** |  | [optional] 
**contact_phone** | **str** |  | [optional] 
**created_by** | **str** |  | 
**admin_email** | **str** |  | 
**status** | **str** |  | 
**sector** | **str** |  | [optional] 
**created_at** | **datetime** |  | 
**updated_at** | **datetime** |  | 
**updated_by** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.get_organisation_response_content import GetOrganisationResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of GetOrganisationResponseContent from a JSON string
get_organisation_response_content_instance = GetOrganisationResponseContent.from_json(json)
# print the JSON string representation of the object
print(GetOrganisationResponseContent.to_json())

# convert the object into a dict
get_organisation_response_content_dict = get_organisation_response_content_instance.to_dict()
# create an instance of GetOrganisationResponseContent from a dict
get_organisation_response_content_from_dict = GetOrganisationResponseContent.from_dict(get_organisation_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


