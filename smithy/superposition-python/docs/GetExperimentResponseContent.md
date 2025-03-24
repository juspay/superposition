# GetExperimentResponseContent


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
from superposition_sdk_python.models.get_experiment_response_content import GetExperimentResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of GetExperimentResponseContent from a JSON string
get_experiment_response_content_instance = GetExperimentResponseContent.from_json(json)
# print the JSON string representation of the object
print(GetExperimentResponseContent.to_json())

# convert the object into a dict
get_experiment_response_content_dict = get_experiment_response_content_instance.to_dict()
# create an instance of GetExperimentResponseContent from a dict
get_experiment_response_content_from_dict = GetExperimentResponseContent.from_dict(get_experiment_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


