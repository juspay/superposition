# CreateTypeTemplatesRequestContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**TypeName** | **string** |  | 
**TypeSchema** | **interface{}** |  | 
**Description** | **string** |  | 
**ChangeReason** | **string** |  | 

## Methods

### NewCreateTypeTemplatesRequestContent

`func NewCreateTypeTemplatesRequestContent(typeName string, typeSchema interface{}, description string, changeReason string, ) *CreateTypeTemplatesRequestContent`

NewCreateTypeTemplatesRequestContent instantiates a new CreateTypeTemplatesRequestContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewCreateTypeTemplatesRequestContentWithDefaults

`func NewCreateTypeTemplatesRequestContentWithDefaults() *CreateTypeTemplatesRequestContent`

NewCreateTypeTemplatesRequestContentWithDefaults instantiates a new CreateTypeTemplatesRequestContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetTypeName

`func (o *CreateTypeTemplatesRequestContent) GetTypeName() string`

GetTypeName returns the TypeName field if non-nil, zero value otherwise.

### GetTypeNameOk

`func (o *CreateTypeTemplatesRequestContent) GetTypeNameOk() (*string, bool)`

GetTypeNameOk returns a tuple with the TypeName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTypeName

`func (o *CreateTypeTemplatesRequestContent) SetTypeName(v string)`

SetTypeName sets TypeName field to given value.


### GetTypeSchema

`func (o *CreateTypeTemplatesRequestContent) GetTypeSchema() interface{}`

GetTypeSchema returns the TypeSchema field if non-nil, zero value otherwise.

### GetTypeSchemaOk

`func (o *CreateTypeTemplatesRequestContent) GetTypeSchemaOk() (*interface{}, bool)`

GetTypeSchemaOk returns a tuple with the TypeSchema field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTypeSchema

`func (o *CreateTypeTemplatesRequestContent) SetTypeSchema(v interface{})`

SetTypeSchema sets TypeSchema field to given value.


### SetTypeSchemaNil

`func (o *CreateTypeTemplatesRequestContent) SetTypeSchemaNil(b bool)`

 SetTypeSchemaNil sets the value for TypeSchema to be an explicit nil

### UnsetTypeSchema
`func (o *CreateTypeTemplatesRequestContent) UnsetTypeSchema()`

UnsetTypeSchema ensures that no value is present for TypeSchema, not even an explicit nil
### GetDescription

`func (o *CreateTypeTemplatesRequestContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *CreateTypeTemplatesRequestContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *CreateTypeTemplatesRequestContent) SetDescription(v string)`

SetDescription sets Description field to given value.


### GetChangeReason

`func (o *CreateTypeTemplatesRequestContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *CreateTypeTemplatesRequestContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *CreateTypeTemplatesRequestContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


