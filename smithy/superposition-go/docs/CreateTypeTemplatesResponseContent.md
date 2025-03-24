# CreateTypeTemplatesResponseContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**TypeName** | **string** |  | 
**TypeSchema** | **interface{}** |  | 
**Description** | **string** |  | 
**ChangeReason** | **string** |  | 
**CreatedBy** | **string** |  | 
**CreatedAt** | **time.Time** |  | 
**LastModifiedAt** | **time.Time** |  | 
**LastModifiedBy** | **string** |  | 

## Methods

### NewCreateTypeTemplatesResponseContent

`func NewCreateTypeTemplatesResponseContent(typeName string, typeSchema interface{}, description string, changeReason string, createdBy string, createdAt time.Time, lastModifiedAt time.Time, lastModifiedBy string, ) *CreateTypeTemplatesResponseContent`

NewCreateTypeTemplatesResponseContent instantiates a new CreateTypeTemplatesResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewCreateTypeTemplatesResponseContentWithDefaults

`func NewCreateTypeTemplatesResponseContentWithDefaults() *CreateTypeTemplatesResponseContent`

NewCreateTypeTemplatesResponseContentWithDefaults instantiates a new CreateTypeTemplatesResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetTypeName

`func (o *CreateTypeTemplatesResponseContent) GetTypeName() string`

GetTypeName returns the TypeName field if non-nil, zero value otherwise.

### GetTypeNameOk

`func (o *CreateTypeTemplatesResponseContent) GetTypeNameOk() (*string, bool)`

GetTypeNameOk returns a tuple with the TypeName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTypeName

`func (o *CreateTypeTemplatesResponseContent) SetTypeName(v string)`

SetTypeName sets TypeName field to given value.


### GetTypeSchema

`func (o *CreateTypeTemplatesResponseContent) GetTypeSchema() interface{}`

GetTypeSchema returns the TypeSchema field if non-nil, zero value otherwise.

### GetTypeSchemaOk

`func (o *CreateTypeTemplatesResponseContent) GetTypeSchemaOk() (*interface{}, bool)`

GetTypeSchemaOk returns a tuple with the TypeSchema field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTypeSchema

`func (o *CreateTypeTemplatesResponseContent) SetTypeSchema(v interface{})`

SetTypeSchema sets TypeSchema field to given value.


### SetTypeSchemaNil

`func (o *CreateTypeTemplatesResponseContent) SetTypeSchemaNil(b bool)`

 SetTypeSchemaNil sets the value for TypeSchema to be an explicit nil

### UnsetTypeSchema
`func (o *CreateTypeTemplatesResponseContent) UnsetTypeSchema()`

UnsetTypeSchema ensures that no value is present for TypeSchema, not even an explicit nil
### GetDescription

`func (o *CreateTypeTemplatesResponseContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *CreateTypeTemplatesResponseContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *CreateTypeTemplatesResponseContent) SetDescription(v string)`

SetDescription sets Description field to given value.


### GetChangeReason

`func (o *CreateTypeTemplatesResponseContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *CreateTypeTemplatesResponseContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *CreateTypeTemplatesResponseContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.


### GetCreatedBy

`func (o *CreateTypeTemplatesResponseContent) GetCreatedBy() string`

GetCreatedBy returns the CreatedBy field if non-nil, zero value otherwise.

### GetCreatedByOk

`func (o *CreateTypeTemplatesResponseContent) GetCreatedByOk() (*string, bool)`

GetCreatedByOk returns a tuple with the CreatedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedBy

`func (o *CreateTypeTemplatesResponseContent) SetCreatedBy(v string)`

SetCreatedBy sets CreatedBy field to given value.


### GetCreatedAt

`func (o *CreateTypeTemplatesResponseContent) GetCreatedAt() time.Time`

GetCreatedAt returns the CreatedAt field if non-nil, zero value otherwise.

### GetCreatedAtOk

`func (o *CreateTypeTemplatesResponseContent) GetCreatedAtOk() (*time.Time, bool)`

GetCreatedAtOk returns a tuple with the CreatedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedAt

`func (o *CreateTypeTemplatesResponseContent) SetCreatedAt(v time.Time)`

SetCreatedAt sets CreatedAt field to given value.


### GetLastModifiedAt

`func (o *CreateTypeTemplatesResponseContent) GetLastModifiedAt() time.Time`

GetLastModifiedAt returns the LastModifiedAt field if non-nil, zero value otherwise.

### GetLastModifiedAtOk

`func (o *CreateTypeTemplatesResponseContent) GetLastModifiedAtOk() (*time.Time, bool)`

GetLastModifiedAtOk returns a tuple with the LastModifiedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedAt

`func (o *CreateTypeTemplatesResponseContent) SetLastModifiedAt(v time.Time)`

SetLastModifiedAt sets LastModifiedAt field to given value.


### GetLastModifiedBy

`func (o *CreateTypeTemplatesResponseContent) GetLastModifiedBy() string`

GetLastModifiedBy returns the LastModifiedBy field if non-nil, zero value otherwise.

### GetLastModifiedByOk

`func (o *CreateTypeTemplatesResponseContent) GetLastModifiedByOk() (*string, bool)`

GetLastModifiedByOk returns a tuple with the LastModifiedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedBy

`func (o *CreateTypeTemplatesResponseContent) SetLastModifiedBy(v string)`

SetLastModifiedBy sets LastModifiedBy field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


