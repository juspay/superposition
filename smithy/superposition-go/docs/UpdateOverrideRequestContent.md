# UpdateOverrideRequestContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Context** | **map[string]interface{}** |  | 
**Override** | **map[string]interface{}** |  | 
**Description** | Pointer to **string** |  | [optional] 
**ChangeReason** | **string** |  | 

## Methods

### NewUpdateOverrideRequestContent

`func NewUpdateOverrideRequestContent(context map[string]interface{}, override map[string]interface{}, changeReason string, ) *UpdateOverrideRequestContent`

NewUpdateOverrideRequestContent instantiates a new UpdateOverrideRequestContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewUpdateOverrideRequestContentWithDefaults

`func NewUpdateOverrideRequestContentWithDefaults() *UpdateOverrideRequestContent`

NewUpdateOverrideRequestContentWithDefaults instantiates a new UpdateOverrideRequestContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetContext

`func (o *UpdateOverrideRequestContent) GetContext() map[string]interface{}`

GetContext returns the Context field if non-nil, zero value otherwise.

### GetContextOk

`func (o *UpdateOverrideRequestContent) GetContextOk() (*map[string]interface{}, bool)`

GetContextOk returns a tuple with the Context field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetContext

`func (o *UpdateOverrideRequestContent) SetContext(v map[string]interface{})`

SetContext sets Context field to given value.


### GetOverride

`func (o *UpdateOverrideRequestContent) GetOverride() map[string]interface{}`

GetOverride returns the Override field if non-nil, zero value otherwise.

### GetOverrideOk

`func (o *UpdateOverrideRequestContent) GetOverrideOk() (*map[string]interface{}, bool)`

GetOverrideOk returns a tuple with the Override field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOverride

`func (o *UpdateOverrideRequestContent) SetOverride(v map[string]interface{})`

SetOverride sets Override field to given value.


### GetDescription

`func (o *UpdateOverrideRequestContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *UpdateOverrideRequestContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *UpdateOverrideRequestContent) SetDescription(v string)`

SetDescription sets Description field to given value.

### HasDescription

`func (o *UpdateOverrideRequestContent) HasDescription() bool`

HasDescription returns a boolean if a field has been set.

### GetChangeReason

`func (o *UpdateOverrideRequestContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *UpdateOverrideRequestContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *UpdateOverrideRequestContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


