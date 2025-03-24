# WeightRecomputeResponseContent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **str** |  | [optional] 
**condition** | **Dict[str, object]** |  | [optional] 
**old_weight** | **float** |  | [optional] 
**new_weight** | **float** |  | [optional] 

## Example

```python
from superposition_sdk_python.models.weight_recompute_response_content import WeightRecomputeResponseContent

# TODO update the JSON string below
json = "{}"
# create an instance of WeightRecomputeResponseContent from a JSON string
weight_recompute_response_content_instance = WeightRecomputeResponseContent.from_json(json)
# print the JSON string representation of the object
print(WeightRecomputeResponseContent.to_json())

# convert the object into a dict
weight_recompute_response_content_dict = weight_recompute_response_content_instance.to_dict()
# create an instance of WeightRecomputeResponseContent from a dict
weight_recompute_response_content_from_dict = WeightRecomputeResponseContent.from_dict(weight_recompute_response_content_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


