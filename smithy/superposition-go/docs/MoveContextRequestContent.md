# MoveContextRequestContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Context** | **map[string]interface{}** |  | 
**Description** | Pointer to **string** |  | [optional] 
**ChangeReason** | **string** |  | 

## Methods

### NewMoveContextRequestContent

`func NewMoveContextRequestContent(context map[string]interface{}, changeReason string, ) *MoveContextRequestContent`

NewMoveContextRequestContent instantiates a new MoveContextRequestContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewMoveContextRequestContentWithDefaults

`func NewMoveContextRequestContentWithDefaults() *MoveContextRequestContent`

NewMoveContextRequestContentWithDefaults instantiates a new MoveContextRequestContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetContext

`func (o *MoveContextRequestContent) GetContext() map[string]interface{}`

GetContext returns the Context field if non-nil, zero value otherwise.

### GetContextOk

`func (o *MoveContextRequestContent) GetContextOk() (*map[string]interface{}, bool)`

GetContextOk returns a tuple with the Context field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetContext

`func (o *MoveContextRequestContent) SetContext(v map[string]interface{})`

SetContext sets Context field to given value.


### GetDescription

`func (o *MoveContextRequestContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *MoveContextRequestContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *MoveContextRequestContent) SetDescription(v string)`

SetDescription sets Description field to given value.

### HasDescription

`func (o *MoveContextRequestContent) HasDescription() bool`

HasDescription returns a boolean if a field has been set.

### GetChangeReason

`func (o *MoveContextRequestContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *MoveContextRequestContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *MoveContextRequestContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


