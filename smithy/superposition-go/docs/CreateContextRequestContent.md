# CreateContextRequestContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Context** | **map[string]interface{}** |  | 
**Override** | **map[string]interface{}** |  | 
**Description** | Pointer to **string** |  | [optional] 
**ChangeReason** | **string** |  | 

## Methods

### NewCreateContextRequestContent

`func NewCreateContextRequestContent(context map[string]interface{}, override map[string]interface{}, changeReason string, ) *CreateContextRequestContent`

NewCreateContextRequestContent instantiates a new CreateContextRequestContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewCreateContextRequestContentWithDefaults

`func NewCreateContextRequestContentWithDefaults() *CreateContextRequestContent`

NewCreateContextRequestContentWithDefaults instantiates a new CreateContextRequestContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetContext

`func (o *CreateContextRequestContent) GetContext() map[string]interface{}`

GetContext returns the Context field if non-nil, zero value otherwise.

### GetContextOk

`func (o *CreateContextRequestContent) GetContextOk() (*map[string]interface{}, bool)`

GetContextOk returns a tuple with the Context field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetContext

`func (o *CreateContextRequestContent) SetContext(v map[string]interface{})`

SetContext sets Context field to given value.


### GetOverride

`func (o *CreateContextRequestContent) GetOverride() map[string]interface{}`

GetOverride returns the Override field if non-nil, zero value otherwise.

### GetOverrideOk

`func (o *CreateContextRequestContent) GetOverrideOk() (*map[string]interface{}, bool)`

GetOverrideOk returns a tuple with the Override field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOverride

`func (o *CreateContextRequestContent) SetOverride(v map[string]interface{})`

SetOverride sets Override field to given value.


### GetDescription

`func (o *CreateContextRequestContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *CreateContextRequestContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *CreateContextRequestContent) SetDescription(v string)`

SetDescription sets Description field to given value.

### HasDescription

`func (o *CreateContextRequestContent) HasDescription() bool`

HasDescription returns a boolean if a field has been set.

### GetChangeReason

`func (o *CreateContextRequestContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *CreateContextRequestContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *CreateContextRequestContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


