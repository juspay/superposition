# ExperimentResponse


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
from superposition_sdk_python.models.experiment_response import ExperimentResponse

# TODO update the JSON string below
json = "{}"
# create an instance of ExperimentResponse from a JSON string
experiment_response_instance = ExperimentResponse.from_json(json)
# print the JSON string representation of the object
print(ExperimentResponse.to_json())

# convert the object into a dict
experiment_response_dict = experiment_response_instance.to_dict()
# create an instance of ExperimentResponse from a dict
experiment_response_from_dict = ExperimentResponse.from_dict(experiment_response_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


