# UpdateTypeTemplatesRequestContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**TypeSchema** | **interface{}** |  | 
**Description** | Pointer to **string** |  | [optional] 
**ChangeReason** | **string** |  | 

## Methods

### NewUpdateTypeTemplatesRequestContent

`func NewUpdateTypeTemplatesRequestContent(typeSchema interface{}, changeReason string, ) *UpdateTypeTemplatesRequestContent`

NewUpdateTypeTemplatesRequestContent instantiates a new UpdateTypeTemplatesRequestContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewUpdateTypeTemplatesRequestContentWithDefaults

`func NewUpdateTypeTemplatesRequestContentWithDefaults() *UpdateTypeTemplatesRequestContent`

NewUpdateTypeTemplatesRequestContentWithDefaults instantiates a new UpdateTypeTemplatesRequestContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetTypeSchema

`func (o *UpdateTypeTemplatesRequestContent) GetTypeSchema() interface{}`

GetTypeSchema returns the TypeSchema field if non-nil, zero value otherwise.

### GetTypeSchemaOk

`func (o *UpdateTypeTemplatesRequestContent) GetTypeSchemaOk() (*interface{}, bool)`

GetTypeSchemaOk returns a tuple with the TypeSchema field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTypeSchema

`func (o *UpdateTypeTemplatesRequestContent) SetTypeSchema(v interface{})`

SetTypeSchema sets TypeSchema field to given value.


### SetTypeSchemaNil

`func (o *UpdateTypeTemplatesRequestContent) SetTypeSchemaNil(b bool)`

 SetTypeSchemaNil sets the value for TypeSchema to be an explicit nil

### UnsetTypeSchema
`func (o *UpdateTypeTemplatesRequestContent) UnsetTypeSchema()`

UnsetTypeSchema ensures that no value is present for TypeSchema, not even an explicit nil
### GetDescription

`func (o *UpdateTypeTemplatesRequestContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *UpdateTypeTemplatesRequestContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *UpdateTypeTemplatesRequestContent) SetDescription(v string)`

SetDescription sets Description field to given value.

### HasDescription

`func (o *UpdateTypeTemplatesRequestContent) HasDescription() bool`

HasDescription returns a boolean if a field has been set.

### GetChangeReason

`func (o *UpdateTypeTemplatesRequestContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *UpdateTypeTemplatesRequestContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *UpdateTypeTemplatesRequestContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


