# CreateExperimentRequestContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **str** |  | 
**context** | **Dict[str, object]** |  | 
**variants** | [**List[Variant]**](Variant.md) |  | 
**description** | **str** |  | 
**change_reason** | **str** |  | 

## Example

```python
from superposition_sdk_python.models.create_experiment_request_content import CreateExperimentRequestContent

# TODO update the JSON string below
json = "{}"
# create an instance of CreateExperimentRequestContent from a JSON string
create_experiment_request_content_instance = CreateExperimentRequestContent.from_json(json)
# print the JSON string representation of the object
print(CreateExperimentRequestContent.to_json())

# convert the object into a dict
create_experiment_request_content_dict = create_experiment_request_content_instance.to_dict()
# create an instance of CreateExperimentRequestContent from a dict
create_experiment_request_content_from_dict = CreateExperimentRequestContent.from_dict(create_experiment_request_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


