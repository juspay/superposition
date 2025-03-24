# CreaterOrganisationResponseContent


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
from superposition_sdk_python.models.creater_organisation_response_content import CreaterOrganisationResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of CreaterOrganisationResponseContent from a JSON string
creater_organisation_response_content_instance = CreaterOrganisationResponseContent.from_json(json)
# print the JSON string representation of the object
print(CreaterOrganisationResponseContent.to_json())

# convert the object into a dict
creater_organisation_response_content_dict = creater_organisation_response_content_instance.to_dict()
# create an instance of CreaterOrganisationResponseContent from a dict
creater_organisation_response_content_from_dict = CreaterOrganisationResponseContent.from_dict(creater_organisation_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


