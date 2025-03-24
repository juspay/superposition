# ListExperimentResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**content** | [**ExperimentListResponse**](ExperimentListResponse.md) |  | [optional] 

## Example

```python
from superposition_sdk_python.models.list_experiment_response_content import ListExperimentResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of ListExperimentResponseContent from a JSON string
list_experiment_response_content_instance = ListExperimentResponseContent.from_json(json)
# print the JSON string representation of the object
print(ListExperimentResponseContent.to_json())

# convert the object into a dict
list_experiment_response_content_dict = list_experiment_response_content_instance.to_dict()
# create an instance of ListExperimentResponseContent from a dict
list_experiment_response_content_from_dict = ListExperimentResponseContent.from_dict(list_experiment_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


