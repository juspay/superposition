# UpdateFunctionRequestContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Description** | Pointer to **string** |  | [optional] 
**ChangeReason** | **string** |  | 
**Function** | **string** |  | 
**RuntimeVersion** | **string** |  | 

## Methods

### NewUpdateFunctionRequestContent

`func NewUpdateFunctionRequestContent(changeReason string, function string, runtimeVersion string, ) *UpdateFunctionRequestContent`

NewUpdateFunctionRequestContent instantiates a new UpdateFunctionRequestContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewUpdateFunctionRequestContentWithDefaults

`func NewUpdateFunctionRequestContentWithDefaults() *UpdateFunctionRequestContent`

NewUpdateFunctionRequestContentWithDefaults instantiates a new UpdateFunctionRequestContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetDescription

`func (o *UpdateFunctionRequestContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *UpdateFunctionRequestContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *UpdateFunctionRequestContent) SetDescription(v string)`

SetDescription sets Description field to given value.

### HasDescription

`func (o *UpdateFunctionRequestContent) HasDescription() bool`

HasDescription returns a boolean if a field has been set.

### GetChangeReason

`func (o *UpdateFunctionRequestContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *UpdateFunctionRequestContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *UpdateFunctionRequestContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.


### GetFunction

`func (o *UpdateFunctionRequestContent) GetFunction() string`

GetFunction returns the Function field if non-nil, zero value otherwise.

### GetFunctionOk

`func (o *UpdateFunctionRequestContent) GetFunctionOk() (*string, bool)`

GetFunctionOk returns a tuple with the Function field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFunction

`func (o *UpdateFunctionRequestContent) SetFunction(v string)`

SetFunction sets Function field to given value.


### GetRuntimeVersion

`func (o *UpdateFunctionRequestContent) GetRuntimeVersion() string`

GetRuntimeVersion returns the RuntimeVersion field if non-nil, zero value otherwise.

### GetRuntimeVersionOk

`func (o *UpdateFunctionRequestContent) GetRuntimeVersionOk() (*string, bool)`

GetRuntimeVersionOk returns a tuple with the RuntimeVersion field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetRuntimeVersion

`func (o *UpdateFunctionRequestContent) SetRuntimeVersion(v string)`

SetRuntimeVersion sets RuntimeVersion field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


