# UpdateTypeTemplatesResponseContent

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

### NewUpdateTypeTemplatesResponseContent

`func NewUpdateTypeTemplatesResponseContent(typeName string, typeSchema interface{}, description string, changeReason string, createdBy string, createdAt time.Time, lastModifiedAt time.Time, lastModifiedBy string, ) *UpdateTypeTemplatesResponseContent`

NewUpdateTypeTemplatesResponseContent instantiates a new UpdateTypeTemplatesResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewUpdateTypeTemplatesResponseContentWithDefaults

`func NewUpdateTypeTemplatesResponseContentWithDefaults() *UpdateTypeTemplatesResponseContent`

NewUpdateTypeTemplatesResponseContentWithDefaults instantiates a new UpdateTypeTemplatesResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetTypeName

`func (o *UpdateTypeTemplatesResponseContent) GetTypeName() string`

GetTypeName returns the TypeName field if non-nil, zero value otherwise.

### GetTypeNameOk

`func (o *UpdateTypeTemplatesResponseContent) GetTypeNameOk() (*string, bool)`

GetTypeNameOk returns a tuple with the TypeName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTypeName

`func (o *UpdateTypeTemplatesResponseContent) SetTypeName(v string)`

SetTypeName sets TypeName field to given value.


### GetTypeSchema

`func (o *UpdateTypeTemplatesResponseContent) GetTypeSchema() interface{}`

GetTypeSchema returns the TypeSchema field if non-nil, zero value otherwise.

### GetTypeSchemaOk

`func (o *UpdateTypeTemplatesResponseContent) GetTypeSchemaOk() (*interface{}, bool)`

GetTypeSchemaOk returns a tuple with the TypeSchema field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTypeSchema

`func (o *UpdateTypeTemplatesResponseContent) SetTypeSchema(v interface{})`

SetTypeSchema sets TypeSchema field to given value.


### SetTypeSchemaNil

`func (o *UpdateTypeTemplatesResponseContent) SetTypeSchemaNil(b bool)`

 SetTypeSchemaNil sets the value for TypeSchema to be an explicit nil

### UnsetTypeSchema
`func (o *UpdateTypeTemplatesResponseContent) UnsetTypeSchema()`

UnsetTypeSchema ensures that no value is present for TypeSchema, not even an explicit nil
### GetDescription

`func (o *UpdateTypeTemplatesResponseContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *UpdateTypeTemplatesResponseContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *UpdateTypeTemplatesResponseContent) SetDescription(v string)`

SetDescription sets Description field to given value.


### GetChangeReason

`func (o *UpdateTypeTemplatesResponseContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *UpdateTypeTemplatesResponseContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *UpdateTypeTemplatesResponseContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.


### GetCreatedBy

`func (o *UpdateTypeTemplatesResponseContent) GetCreatedBy() string`

GetCreatedBy returns the CreatedBy field if non-nil, zero value otherwise.

### GetCreatedByOk

`func (o *UpdateTypeTemplatesResponseContent) GetCreatedByOk() (*string, bool)`

GetCreatedByOk returns a tuple with the CreatedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedBy

`func (o *UpdateTypeTemplatesResponseContent) SetCreatedBy(v string)`

SetCreatedBy sets CreatedBy field to given value.


### GetCreatedAt

`func (o *UpdateTypeTemplatesResponseContent) GetCreatedAt() time.Time`

GetCreatedAt returns the CreatedAt field if non-nil, zero value otherwise.

### GetCreatedAtOk

`func (o *UpdateTypeTemplatesResponseContent) GetCreatedAtOk() (*time.Time, bool)`

GetCreatedAtOk returns a tuple with the CreatedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedAt

`func (o *UpdateTypeTemplatesResponseContent) SetCreatedAt(v time.Time)`

SetCreatedAt sets CreatedAt field to given value.


### GetLastModifiedAt

`func (o *UpdateTypeTemplatesResponseContent) GetLastModifiedAt() time.Time`

GetLastModifiedAt returns the LastModifiedAt field if non-nil, zero value otherwise.

### GetLastModifiedAtOk

`func (o *UpdateTypeTemplatesResponseContent) GetLastModifiedAtOk() (*time.Time, bool)`

GetLastModifiedAtOk returns a tuple with the LastModifiedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedAt

`func (o *UpdateTypeTemplatesResponseContent) SetLastModifiedAt(v time.Time)`

SetLastModifiedAt sets LastModifiedAt field to given value.


### GetLastModifiedBy

`func (o *UpdateTypeTemplatesResponseContent) GetLastModifiedBy() string`

GetLastModifiedBy returns the LastModifiedBy field if non-nil, zero value otherwise.

### GetLastModifiedByOk

`func (o *UpdateTypeTemplatesResponseContent) GetLastModifiedByOk() (*string, bool)`

GetLastModifiedByOk returns a tuple with the LastModifiedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedBy

`func (o *UpdateTypeTemplatesResponseContent) SetLastModifiedBy(v string)`

SetLastModifiedBy sets LastModifiedBy field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


