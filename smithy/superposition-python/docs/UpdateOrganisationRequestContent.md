# UpdateOrganisationRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**country_code** | **str** |  | [optional] 
**contact_email** | **str** |  | [optional] 
**contact_phone** | **str** |  | [optional] 
**admin_email** | **str** |  | [optional] 
**sector** | **str** |  | [optional] 
**status** | **str** |  | [optional] 

## Example

```python
from superposition_sdk_python.models.update_organisation_request_content import UpdateOrganisationRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of UpdateOrganisationRequestContent from a JSON string
update_organisation_request_content_instance = UpdateOrganisationRequestContent.from_json(json)
# print the JSON string representation of the object
print(UpdateOrganisationRequestContent.to_json())

# convert the object into a dict
update_organisation_request_content_dict = update_organisation_request_content_instance.to_dict()
# create an instance of UpdateOrganisationRequestContent from a dict
update_organisation_request_content_from_dict = UpdateOrganisationRequestContent.from_dict(update_organisation_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


