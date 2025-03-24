# ExperimentListResponse


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**total_pages** | **float** |  | 
**total_items** | **float** |  | 
**data** | [**List[ExperimentResponse]**](ExperimentResponse.md) |  | 

## Example

```python
from superposition_sdk_python.models.experiment_list_response import ExperimentListResponse

# TODO update the JSON string below
json = "{}"
# create an instance of ExperimentListResponse from a JSON string
experiment_list_response_instance = ExperimentListResponse.from_json(json)
# print the JSON string representation of the object
print(ExperimentListResponse.to_json())

# convert the object into a dict
experiment_list_response_dict = experiment_list_response_instance.to_dict()
# create an instance of ExperimentListResponse from a dict
experiment_list_response_from_dict = ExperimentListResponse.from_dict(experiment_list_response_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


