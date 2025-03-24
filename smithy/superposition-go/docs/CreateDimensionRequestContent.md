# CreateDimensionRequestContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Dimension** | **string** |  | 
**Position** | **float32** |  | 
**Schema** | **interface{}** |  | 
**FunctionName** | Pointer to **string** |  | [optional] 
**Description** | **string** |  | 
**ChangeReason** | **string** |  | 

## Methods

### NewCreateDimensionRequestContent

`func NewCreateDimensionRequestContent(dimension string, position float32, schema interface{}, description string, changeReason string, ) *CreateDimensionRequestContent`

NewCreateDimensionRequestContent instantiates a new CreateDimensionRequestContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewCreateDimensionRequestContentWithDefaults

`func NewCreateDimensionRequestContentWithDefaults() *CreateDimensionRequestContent`

NewCreateDimensionRequestContentWithDefaults instantiates a new CreateDimensionRequestContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetDimension

`func (o *CreateDimensionRequestContent) GetDimension() string`

GetDimension returns the Dimension field if non-nil, zero value otherwise.

### GetDimensionOk

`func (o *CreateDimensionRequestContent) GetDimensionOk() (*string, bool)`

GetDimensionOk returns a tuple with the Dimension field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDimension

`func (o *CreateDimensionRequestContent) SetDimension(v string)`

SetDimension sets Dimension field to given value.


### GetPosition

`func (o *CreateDimensionRequestContent) GetPosition() float32`

GetPosition returns the Position field if non-nil, zero value otherwise.

### GetPositionOk

`func (o *CreateDimensionRequestContent) GetPositionOk() (*float32, bool)`

GetPositionOk returns a tuple with the Position field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPosition

`func (o *CreateDimensionRequestContent) SetPosition(v float32)`

SetPosition sets Position field to given value.


### GetSchema

`func (o *CreateDimensionRequestContent) GetSchema() interface{}`

GetSchema returns the Schema field if non-nil, zero value otherwise.

### GetSchemaOk

`func (o *CreateDimensionRequestContent) GetSchemaOk() (*interface{}, bool)`

GetSchemaOk returns a tuple with the Schema field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetSchema

`func (o *CreateDimensionRequestContent) SetSchema(v interface{})`

SetSchema sets Schema field to given value.


### SetSchemaNil

`func (o *CreateDimensionRequestContent) SetSchemaNil(b bool)`

 SetSchemaNil sets the value for Schema to be an explicit nil

### UnsetSchema
`func (o *CreateDimensionRequestContent) UnsetSchema()`

UnsetSchema ensures that no value is present for Schema, not even an explicit nil
### GetFunctionName

`func (o *CreateDimensionRequestContent) GetFunctionName() string`

GetFunctionName returns the FunctionName field if non-nil, zero value otherwise.

### GetFunctionNameOk

`func (o *CreateDimensionRequestContent) GetFunctionNameOk() (*string, bool)`

GetFunctionNameOk returns a tuple with the FunctionName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFunctionName

`func (o *CreateDimensionRequestContent) SetFunctionName(v string)`

SetFunctionName sets FunctionName field to given value.

### HasFunctionName

`func (o *CreateDimensionRequestContent) HasFunctionName() bool`

HasFunctionName returns a boolean if a field has been set.

### GetDescription

`func (o *CreateDimensionRequestContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *CreateDimensionRequestContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *CreateDimensionRequestContent) SetDescription(v string)`

SetDescription sets Description field to given value.


### GetChangeReason

`func (o *CreateDimensionRequestContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *CreateDimensionRequestContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *CreateDimensionRequestContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


