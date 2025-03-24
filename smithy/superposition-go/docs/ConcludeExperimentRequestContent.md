# ConcludeExperimentRequestContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ChosenVariant** | **string** |  | 
**Description** | Pointer to **string** |  | [optional] 
**ChangeReason** | **string** |  | 

## Methods

### NewConcludeExperimentRequestContent

`func NewConcludeExperimentRequestContent(chosenVariant string, changeReason string, ) *ConcludeExperimentRequestContent`

NewConcludeExperimentRequestContent instantiates a new ConcludeExperimentRequestContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewConcludeExperimentRequestContentWithDefaults

`func NewConcludeExperimentRequestContentWithDefaults() *ConcludeExperimentRequestContent`

NewConcludeExperimentRequestContentWithDefaults instantiates a new ConcludeExperimentRequestContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetChosenVariant

`func (o *ConcludeExperimentRequestContent) GetChosenVariant() string`

GetChosenVariant returns the ChosenVariant field if non-nil, zero value otherwise.

### GetChosenVariantOk

`func (o *ConcludeExperimentRequestContent) GetChosenVariantOk() (*string, bool)`

GetChosenVariantOk returns a tuple with the ChosenVariant field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChosenVariant

`func (o *ConcludeExperimentRequestContent) SetChosenVariant(v string)`

SetChosenVariant sets ChosenVariant field to given value.


### GetDescription

`func (o *ConcludeExperimentRequestContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *ConcludeExperimentRequestContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *ConcludeExperimentRequestContent) SetDescription(v string)`

SetDescription sets Description field to given value.

### HasDescription

`func (o *ConcludeExperimentRequestContent) HasDescription() bool`

HasDescription returns a boolean if a field has been set.

### GetChangeReason

`func (o *ConcludeExperimentRequestContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *ConcludeExperimentRequestContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *ConcludeExperimentRequestContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


