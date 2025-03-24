# CreaterOrganisationRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**country_code** | **str** |  | [optional] 
**contact_email** | **str** |  | [optional] 
**contact_phone** | **str** |  | [optional] 
**admin_email** | **str** |  | 
**name** | **str** |  | 
**sector** | **str** |  | [optional] 

## Example

```python
from superposition_sdk_python.models.creater_organisation_request_content import CreaterOrganisationRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of CreaterOrganisationRequestContent from a JSON string
creater_organisation_request_content_instance = CreaterOrganisationRequestContent.from_json(json)
# print the JSON string representation of the object
print(CreaterOrganisationRequestContent.to_json())

# convert the object into a dict
creater_organisation_request_content_dict = creater_organisation_request_content_instance.to_dict()
# create an instance of CreaterOrganisationRequestContent from a dict
creater_organisation_request_content_from_dict = CreaterOrganisationRequestContent.from_dict(creater_organisation_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


