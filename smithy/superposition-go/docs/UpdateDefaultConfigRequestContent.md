# UpdateDefaultConfigRequestContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ChangeReason** | **string** |  | 
**Value** | Pointer to **interface{}** |  | [optional] 
**Schema** | Pointer to **interface{}** |  | [optional] 
**FunctionName** | Pointer to **string** |  | [optional] 
**Description** | Pointer to **string** |  | [optional] 

## Methods

### NewUpdateDefaultConfigRequestContent

`func NewUpdateDefaultConfigRequestContent(changeReason string, ) *UpdateDefaultConfigRequestContent`

NewUpdateDefaultConfigRequestContent instantiates a new UpdateDefaultConfigRequestContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewUpdateDefaultConfigRequestContentWithDefaults

`func NewUpdateDefaultConfigRequestContentWithDefaults() *UpdateDefaultConfigRequestContent`

NewUpdateDefaultConfigRequestContentWithDefaults instantiates a new UpdateDefaultConfigRequestContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetChangeReason

`func (o *UpdateDefaultConfigRequestContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *UpdateDefaultConfigRequestContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *UpdateDefaultConfigRequestContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.


### GetValue

`func (o *UpdateDefaultConfigRequestContent) GetValue() interface{}`

GetValue returns the Value field if non-nil, zero value otherwise.

### GetValueOk

`func (o *UpdateDefaultConfigRequestContent) GetValueOk() (*interface{}, bool)`

GetValueOk returns a tuple with the Value field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetValue

`func (o *UpdateDefaultConfigRequestContent) SetValue(v interface{})`

SetValue sets Value field to given value.

### HasValue

`func (o *UpdateDefaultConfigRequestContent) HasValue() bool`

HasValue returns a boolean if a field has been set.

### SetValueNil

`func (o *UpdateDefaultConfigRequestContent) SetValueNil(b bool)`

 SetValueNil sets the value for Value to be an explicit nil

### UnsetValue
`func (o *UpdateDefaultConfigRequestContent) UnsetValue()`

UnsetValue ensures that no value is present for Value, not even an explicit nil
### GetSchema

`func (o *UpdateDefaultConfigRequestContent) GetSchema() interface{}`

GetSchema returns the Schema field if non-nil, zero value otherwise.

### GetSchemaOk

`func (o *UpdateDefaultConfigRequestContent) GetSchemaOk() (*interface{}, bool)`

GetSchemaOk returns a tuple with the Schema field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetSchema

`func (o *UpdateDefaultConfigRequestContent) SetSchema(v interface{})`

SetSchema sets Schema field to given value.

### HasSchema

`func (o *UpdateDefaultConfigRequestContent) HasSchema() bool`

HasSchema returns a boolean if a field has been set.

### SetSchemaNil

`func (o *UpdateDefaultConfigRequestContent) SetSchemaNil(b bool)`

 SetSchemaNil sets the value for Schema to be an explicit nil

### UnsetSchema
`func (o *UpdateDefaultConfigRequestContent) UnsetSchema()`

UnsetSchema ensures that no value is present for Schema, not even an explicit nil
### GetFunctionName

`func (o *UpdateDefaultConfigRequestContent) GetFunctionName() string`

GetFunctionName returns the FunctionName field if non-nil, zero value otherwise.

### GetFunctionNameOk

`func (o *UpdateDefaultConfigRequestContent) GetFunctionNameOk() (*string, bool)`

GetFunctionNameOk returns a tuple with the FunctionName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFunctionName

`func (o *UpdateDefaultConfigRequestContent) SetFunctionName(v string)`

SetFunctionName sets FunctionName field to given value.

### HasFunctionName

`func (o *UpdateDefaultConfigRequestContent) HasFunctionName() bool`

HasFunctionName returns a boolean if a field has been set.

### GetDescription

`func (o *UpdateDefaultConfigRequestContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *UpdateDefaultConfigRequestContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *UpdateDefaultConfigRequestContent) SetDescription(v string)`

SetDescription sets Description field to given value.

### HasDescription

`func (o *UpdateDefaultConfigRequestContent) HasDescription() bool`

HasDescription returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


