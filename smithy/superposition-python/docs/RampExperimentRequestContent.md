# RampExperimentRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**change_reason** | **str** |  | 
**traffic_percentage** | **float** |  | 

## Example

```python
from superposition_sdk_python.models.ramp_experiment_request_content import RampExperimentRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of RampExperimentRequestContent from a JSON string
ramp_experiment_request_content_instance = RampExperimentRequestContent.from_json(json)
# print the JSON string representation of the object
print(RampExperimentRequestContent.to_json())

# convert the object into a dict
ramp_experiment_request_content_dict = ramp_experiment_request_content_instance.to_dict()
# create an instance of RampExperimentRequestContent from a dict
ramp_experiment_request_content_from_dict = RampExperimentRequestContent.from_dict(ramp_experiment_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


