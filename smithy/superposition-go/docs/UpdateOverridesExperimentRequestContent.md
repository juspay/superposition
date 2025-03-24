# UpdateOverridesExperimentRequestContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Variants** | [**[]Variant**](Variant.md) |  | 
**Description** | Pointer to **string** |  | [optional] 
**ChangeReason** | **string** |  | 

## Methods

### NewUpdateOverridesExperimentRequestContent

`func NewUpdateOverridesExperimentRequestContent(variants []Variant, changeReason string, ) *UpdateOverridesExperimentRequestContent`

NewUpdateOverridesExperimentRequestContent instantiates a new UpdateOverridesExperimentRequestContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewUpdateOverridesExperimentRequestContentWithDefaults

`func NewUpdateOverridesExperimentRequestContentWithDefaults() *UpdateOverridesExperimentRequestContent`

NewUpdateOverridesExperimentRequestContentWithDefaults instantiates a new UpdateOverridesExperimentRequestContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetVariants

`func (o *UpdateOverridesExperimentRequestContent) GetVariants() []Variant`

GetVariants returns the Variants field if non-nil, zero value otherwise.

### GetVariantsOk

`func (o *UpdateOverridesExperimentRequestContent) GetVariantsOk() (*[]Variant, bool)`

GetVariantsOk returns a tuple with the Variants field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetVariants

`func (o *UpdateOverridesExperimentRequestContent) SetVariants(v []Variant)`

SetVariants sets Variants field to given value.


### GetDescription

`func (o *UpdateOverridesExperimentRequestContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *UpdateOverridesExperimentRequestContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *UpdateOverridesExperimentRequestContent) SetDescription(v string)`

SetDescription sets Description field to given value.

### HasDescription

`func (o *UpdateOverridesExperimentRequestContent) HasDescription() bool`

HasDescription returns a boolean if a field has been set.

### GetChangeReason

`func (o *UpdateOverridesExperimentRequestContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *UpdateOverridesExperimentRequestContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *UpdateOverridesExperimentRequestContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


