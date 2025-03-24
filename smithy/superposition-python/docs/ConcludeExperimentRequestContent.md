# ConcludeExperimentRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**chosen_variant** | **str** |  | 
**description** | **str** |  | [optional] 
**change_reason** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.conclude_experiment_request_content import ConcludeExperimentRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of ConcludeExperimentRequestContent from a JSON string
conclude_experiment_request_content_instance = ConcludeExperimentRequestContent.from_json(json)
# print the JSON string representation of the object
print(ConcludeExperimentRequestContent.to_json())

# convert the object into a dict
conclude_experiment_request_content_dict = conclude_experiment_request_content_instance.to_dict()
# create an instance of ConcludeExperimentRequestContent from a dict
conclude_experiment_request_content_from_dict = ConcludeExperimentRequestContent.from_dict(conclude_experiment_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


