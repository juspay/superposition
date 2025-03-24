# UpdateOrganisationResponseContent


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
from superposition_sdk_python.models.update_organisation_response_content import UpdateOrganisationResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of UpdateOrganisationResponseContent from a JSON string
update_organisation_response_content_instance = UpdateOrganisationResponseContent.from_json(json)
# print the JSON string representation of the object
print(UpdateOrganisationResponseContent.to_json())

# convert the object into a dict
update_organisation_response_content_dict = update_organisation_response_content_instance.to_dict()
# create an instance of UpdateOrganisationResponseContent from a dict
update_organisation_response_content_from_dict = UpdateOrganisationResponseContent.from_dict(update_organisation_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


