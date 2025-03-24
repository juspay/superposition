# UpdateDefaultConfigResponseContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Key** | **string** |  | 
**Value** | **interface{}** |  | 
**Schema** | **interface{}** |  | 
**Description** | **string** |  | 
**ChangeReason** | **string** |  | 
**FunctionName** | Pointer to **string** | Optional | [optional] 
**CreatedAt** | **time.Time** |  | 
**CreatedBy** | **string** |  | 
**LastModifiedAt** | **time.Time** |  | 
**LastModifiedBy** | **string** |  | 

## Methods

### NewUpdateDefaultConfigResponseContent

`func NewUpdateDefaultConfigResponseContent(key string, value interface{}, schema interface{}, description string, changeReason string, createdAt time.Time, createdBy string, lastModifiedAt time.Time, lastModifiedBy string, ) *UpdateDefaultConfigResponseContent`

NewUpdateDefaultConfigResponseContent instantiates a new UpdateDefaultConfigResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewUpdateDefaultConfigResponseContentWithDefaults

`func NewUpdateDefaultConfigResponseContentWithDefaults() *UpdateDefaultConfigResponseContent`

NewUpdateDefaultConfigResponseContentWithDefaults instantiates a new UpdateDefaultConfigResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetKey

`func (o *UpdateDefaultConfigResponseContent) GetKey() string`

GetKey returns the Key field if non-nil, zero value otherwise.

### GetKeyOk

`func (o *UpdateDefaultConfigResponseContent) GetKeyOk() (*string, bool)`

GetKeyOk returns a tuple with the Key field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetKey

`func (o *UpdateDefaultConfigResponseContent) SetKey(v string)`

SetKey sets Key field to given value.


### GetValue

`func (o *UpdateDefaultConfigResponseContent) GetValue() interface{}`

GetValue returns the Value field if non-nil, zero value otherwise.

### GetValueOk

`func (o *UpdateDefaultConfigResponseContent) GetValueOk() (*interface{}, bool)`

GetValueOk returns a tuple with the Value field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetValue

`func (o *UpdateDefaultConfigResponseContent) SetValue(v interface{})`

SetValue sets Value field to given value.


### SetValueNil

`func (o *UpdateDefaultConfigResponseContent) SetValueNil(b bool)`

 SetValueNil sets the value for Value to be an explicit nil

### UnsetValue
`func (o *UpdateDefaultConfigResponseContent) UnsetValue()`

UnsetValue ensures that no value is present for Value, not even an explicit nil
### GetSchema

`func (o *UpdateDefaultConfigResponseContent) GetSchema() interface{}`

GetSchema returns the Schema field if non-nil, zero value otherwise.

### GetSchemaOk

`func (o *UpdateDefaultConfigResponseContent) GetSchemaOk() (*interface{}, bool)`

GetSchemaOk returns a tuple with the Schema field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetSchema

`func (o *UpdateDefaultConfigResponseContent) SetSchema(v interface{})`

SetSchema sets Schema field to given value.


### SetSchemaNil

`func (o *UpdateDefaultConfigResponseContent) SetSchemaNil(b bool)`

 SetSchemaNil sets the value for Schema to be an explicit nil

### UnsetSchema
`func (o *UpdateDefaultConfigResponseContent) UnsetSchema()`

UnsetSchema ensures that no value is present for Schema, not even an explicit nil
### GetDescription

`func (o *UpdateDefaultConfigResponseContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *UpdateDefaultConfigResponseContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *UpdateDefaultConfigResponseContent) SetDescription(v string)`

SetDescription sets Description field to given value.


### GetChangeReason

`func (o *UpdateDefaultConfigResponseContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *UpdateDefaultConfigResponseContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *UpdateDefaultConfigResponseContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.


### GetFunctionName

`func (o *UpdateDefaultConfigResponseContent) GetFunctionName() string`

GetFunctionName returns the FunctionName field if non-nil, zero value otherwise.

### GetFunctionNameOk

`func (o *UpdateDefaultConfigResponseContent) GetFunctionNameOk() (*string, bool)`

GetFunctionNameOk returns a tuple with the FunctionName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFunctionName

`func (o *UpdateDefaultConfigResponseContent) SetFunctionName(v string)`

SetFunctionName sets FunctionName field to given value.

### HasFunctionName

`func (o *UpdateDefaultConfigResponseContent) HasFunctionName() bool`

HasFunctionName returns a boolean if a field has been set.

### GetCreatedAt

`func (o *UpdateDefaultConfigResponseContent) GetCreatedAt() time.Time`

GetCreatedAt returns the CreatedAt field if non-nil, zero value otherwise.

### GetCreatedAtOk

`func (o *UpdateDefaultConfigResponseContent) GetCreatedAtOk() (*time.Time, bool)`

GetCreatedAtOk returns a tuple with the CreatedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedAt

`func (o *UpdateDefaultConfigResponseContent) SetCreatedAt(v time.Time)`

SetCreatedAt sets CreatedAt field to given value.


### GetCreatedBy

`func (o *UpdateDefaultConfigResponseContent) GetCreatedBy() string`

GetCreatedBy returns the CreatedBy field if non-nil, zero value otherwise.

### GetCreatedByOk

`func (o *UpdateDefaultConfigResponseContent) GetCreatedByOk() (*string, bool)`

GetCreatedByOk returns a tuple with the CreatedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedBy

`func (o *UpdateDefaultConfigResponseContent) SetCreatedBy(v string)`

SetCreatedBy sets CreatedBy field to given value.


### GetLastModifiedAt

`func (o *UpdateDefaultConfigResponseContent) GetLastModifiedAt() time.Time`

GetLastModifiedAt returns the LastModifiedAt field if non-nil, zero value otherwise.

### GetLastModifiedAtOk

`func (o *UpdateDefaultConfigResponseContent) GetLastModifiedAtOk() (*time.Time, bool)`

GetLastModifiedAtOk returns a tuple with the LastModifiedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedAt

`func (o *UpdateDefaultConfigResponseContent) SetLastModifiedAt(v time.Time)`

SetLastModifiedAt sets LastModifiedAt field to given value.


### GetLastModifiedBy

`func (o *UpdateDefaultConfigResponseContent) GetLastModifiedBy() string`

GetLastModifiedBy returns the LastModifiedBy field if non-nil, zero value otherwise.

### GetLastModifiedByOk

`func (o *UpdateDefaultConfigResponseContent) GetLastModifiedByOk() (*string, bool)`

GetLastModifiedByOk returns a tuple with the LastModifiedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedBy

`func (o *UpdateDefaultConfigResponseContent) SetLastModifiedBy(v string)`

SetLastModifiedBy sets LastModifiedBy field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


