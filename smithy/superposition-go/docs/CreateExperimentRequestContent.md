# CreateExperimentRequestContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Name** | **string** |  | 
**Context** | **map[string]interface{}** |  | 
**Variants** | [**[]Variant**](Variant.md) |  | 
**Description** | **string** |  | 
**ChangeReason** | **string** |  | 

## Methods

### NewCreateExperimentRequestContent

`func NewCreateExperimentRequestContent(name string, context map[string]interface{}, variants []Variant, description string, changeReason string, ) *CreateExperimentRequestContent`

NewCreateExperimentRequestContent instantiates a new CreateExperimentRequestContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewCreateExperimentRequestContentWithDefaults

`func NewCreateExperimentRequestContentWithDefaults() *CreateExperimentRequestContent`

NewCreateExperimentRequestContentWithDefaults instantiates a new CreateExperimentRequestContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetName

`func (o *CreateExperimentRequestContent) GetName() string`

GetName returns the Name field if non-nil, zero value otherwise.

### GetNameOk

`func (o *CreateExperimentRequestContent) GetNameOk() (*string, bool)`

GetNameOk returns a tuple with the Name field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetName

`func (o *CreateExperimentRequestContent) SetName(v string)`

SetName sets Name field to given value.


### GetContext

`func (o *CreateExperimentRequestContent) GetContext() map[string]interface{}`

GetContext returns the Context field if non-nil, zero value otherwise.

### GetContextOk

`func (o *CreateExperimentRequestContent) GetContextOk() (*map[string]interface{}, bool)`

GetContextOk returns a tuple with the Context field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetContext

`func (o *CreateExperimentRequestContent) SetContext(v map[string]interface{})`

SetContext sets Context field to given value.


### GetVariants

`func (o *CreateExperimentRequestContent) GetVariants() []Variant`

GetVariants returns the Variants field if non-nil, zero value otherwise.

### GetVariantsOk

`func (o *CreateExperimentRequestContent) GetVariantsOk() (*[]Variant, bool)`

GetVariantsOk returns a tuple with the Variants field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetVariants

`func (o *CreateExperimentRequestContent) SetVariants(v []Variant)`

SetVariants sets Variants field to given value.


### GetDescription

`func (o *CreateExperimentRequestContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *CreateExperimentRequestContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *CreateExperimentRequestContent) SetDescription(v string)`

SetDescription sets Description field to given value.


### GetChangeReason

`func (o *CreateExperimentRequestContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *CreateExperimentRequestContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *CreateExperimentRequestContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


