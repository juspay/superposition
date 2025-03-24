# UpdateOverridesExperimentRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**variants** | [**List[Variant]**](Variant.md) |  | 
**description** | **str** |  | [optional] 
**change_reason** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.update_overrides_experiment_request_content import UpdateOverridesExperimentRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of UpdateOverridesExperimentRequestContent from a JSON string
update_overrides_experiment_request_content_instance = UpdateOverridesExperimentRequestContent.from_json(json)
# print the JSON string representation of the object
print(UpdateOverridesExperimentRequestContent.to_json())

# convert the object into a dict
update_overrides_experiment_request_content_dict = update_overrides_experiment_request_content_instance.to_dict()
# create an instance of UpdateOverridesExperimentRequestContent from a dict
update_overrides_experiment_request_content_from_dict = UpdateOverridesExperimentRequestContent.from_dict(update_overrides_experiment_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


