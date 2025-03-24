# OrganisationResponse


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
from superposition_sdk_python.models.organisation_response import OrganisationResponse

# TODO update the JSON string below
json = "{}"
# create an instance of OrganisationResponse from a JSON string
organisation_response_instance = OrganisationResponse.from_json(json)
# print the JSON string representation of the object
print(OrganisationResponse.to_json())

# convert the object into a dict
organisation_response_dict = organisation_response_instance.to_dict()
# create an instance of OrganisationResponse from a dict
organisation_response_from_dict = OrganisationResponse.from_dict(organisation_response_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


