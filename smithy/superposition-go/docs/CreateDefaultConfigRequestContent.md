# CreateDefaultConfigRequestContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Key** | **string** |  | 
**Value** | **interface{}** |  | 
**Schema** | **interface{}** |  | 
**Description** | **string** |  | 
**ChangeReason** | **string** |  | 
**FunctionName** | Pointer to **string** | Optional | [optional] 

## Methods

### NewCreateDefaultConfigRequestContent

`func NewCreateDefaultConfigRequestContent(key string, value interface{}, schema interface{}, description string, changeReason string, ) *CreateDefaultConfigRequestContent`

NewCreateDefaultConfigRequestContent instantiates a new CreateDefaultConfigRequestContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewCreateDefaultConfigRequestContentWithDefaults

`func NewCreateDefaultConfigRequestContentWithDefaults() *CreateDefaultConfigRequestContent`

NewCreateDefaultConfigRequestContentWithDefaults instantiates a new CreateDefaultConfigRequestContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetKey

`func (o *CreateDefaultConfigRequestContent) GetKey() string`

GetKey returns the Key field if non-nil, zero value otherwise.

### GetKeyOk

`func (o *CreateDefaultConfigRequestContent) GetKeyOk() (*string, bool)`

GetKeyOk returns a tuple with the Key field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetKey

`func (o *CreateDefaultConfigRequestContent) SetKey(v string)`

SetKey sets Key field to given value.


### GetValue

`func (o *CreateDefaultConfigRequestContent) GetValue() interface{}`

GetValue returns the Value field if non-nil, zero value otherwise.

### GetValueOk

`func (o *CreateDefaultConfigRequestContent) GetValueOk() (*interface{}, bool)`

GetValueOk returns a tuple with the Value field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetValue

`func (o *CreateDefaultConfigRequestContent) SetValue(v interface{})`

SetValue sets Value field to given value.


### SetValueNil

`func (o *CreateDefaultConfigRequestContent) SetValueNil(b bool)`

 SetValueNil sets the value for Value to be an explicit nil

### UnsetValue
`func (o *CreateDefaultConfigRequestContent) UnsetValue()`

UnsetValue ensures that no value is present for Value, not even an explicit nil
### GetSchema

`func (o *CreateDefaultConfigRequestContent) GetSchema() interface{}`

GetSchema returns the Schema field if non-nil, zero value otherwise.

### GetSchemaOk

`func (o *CreateDefaultConfigRequestContent) GetSchemaOk() (*interface{}, bool)`

GetSchemaOk returns a tuple with the Schema field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetSchema

`func (o *CreateDefaultConfigRequestContent) SetSchema(v interface{})`

SetSchema sets Schema field to given value.


### SetSchemaNil

`func (o *CreateDefaultConfigRequestContent) SetSchemaNil(b bool)`

 SetSchemaNil sets the value for Schema to be an explicit nil

### UnsetSchema
`func (o *CreateDefaultConfigRequestContent) UnsetSchema()`

UnsetSchema ensures that no value is present for Schema, not even an explicit nil
### GetDescription

`func (o *CreateDefaultConfigRequestContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *CreateDefaultConfigRequestContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *CreateDefaultConfigRequestContent) SetDescription(v string)`

SetDescription sets Description field to given value.


### GetChangeReason

`func (o *CreateDefaultConfigRequestContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *CreateDefaultConfigRequestContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *CreateDefaultConfigRequestContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.


### GetFunctionName

`func (o *CreateDefaultConfigRequestContent) GetFunctionName() string`

GetFunctionName returns the FunctionName field if non-nil, zero value otherwise.

### GetFunctionNameOk

`func (o *CreateDefaultConfigRequestContent) GetFunctionNameOk() (*string, bool)`

GetFunctionNameOk returns a tuple with the FunctionName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFunctionName

`func (o *CreateDefaultConfigRequestContent) SetFunctionName(v string)`

SetFunctionName sets FunctionName field to given value.

### HasFunctionName

`func (o *CreateDefaultConfigRequestContent) HasFunctionName() bool`

HasFunctionName returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


