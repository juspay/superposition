# UpdateOverridesExperimentResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **str** |  | 
**created_at** | **datetime** |  | 
**created_by** | **str** |  | 
**last_modified** | **datetime** |  | 
**name** | **str** |  | 
**override_keys** | **List[str]** |  | 
**status** | [**ExperimentStatusType**](ExperimentStatusType.md) |  | 
**traffic_percentage** | **float** |  | 
**context** | **Dict[str, object]** |  | 
**variants** | [**List[Variant]**](Variant.md) |  | 
**last_modified_by** | **str** |  | 
**chosen_variant** | **str** |  | 
**description** | **str** |  | 
**change_reason** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.update_overrides_experiment_response_content import UpdateOverridesExperimentResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of UpdateOverridesExperimentResponseContent from a JSON string
update_overrides_experiment_response_content_instance = UpdateOverridesExperimentResponseContent.from_json(json)
# print the JSON string representation of the object
print(UpdateOverridesExperimentResponseContent.to_json())

# convert the object into a dict
update_overrides_experiment_response_content_dict = update_overrides_experiment_response_content_instance.to_dict()
# create an instance of UpdateOverridesExperimentResponseContent from a dict
update_overrides_experiment_response_content_from_dict = UpdateOverridesExperimentResponseContent.from_dict(update_overrides_experiment_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


