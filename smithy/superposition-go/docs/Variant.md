# Variant

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Id** | **string** |  | 
**VariantType** | **string** |  | 
**ContextId** | Pointer to **string** |  | [optional] 
**OverrideId** | Pointer to **string** |  | [optional] 
**Overrides** | **[]interface{}** |  | 

## Methods

### NewVariant

`func NewVariant(id string, variantType string, overrides []interface{}, ) *Variant`

NewVariant instantiates a new Variant object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewVariantWithDefaults

`func NewVariantWithDefaults() *Variant`

NewVariantWithDefaults instantiates a new Variant object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetId

`func (o *Variant) GetId() string`

GetId returns the Id field if non-nil, zero value otherwise.

### GetIdOk

`func (o *Variant) GetIdOk() (*string, bool)`

GetIdOk returns a tuple with the Id field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetId

`func (o *Variant) SetId(v string)`

SetId sets Id field to given value.


### GetVariantType

`func (o *Variant) GetVariantType() string`

GetVariantType returns the VariantType field if non-nil, zero value otherwise.

### GetVariantTypeOk

`func (o *Variant) GetVariantTypeOk() (*string, bool)`

GetVariantTypeOk returns a tuple with the VariantType field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetVariantType

`func (o *Variant) SetVariantType(v string)`

SetVariantType sets VariantType field to given value.


### GetContextId

`func (o *Variant) GetContextId() string`

GetContextId returns the ContextId field if non-nil, zero value otherwise.

### GetContextIdOk

`func (o *Variant) GetContextIdOk() (*string, bool)`

GetContextIdOk returns a tuple with the ContextId field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetContextId

`func (o *Variant) SetContextId(v string)`

SetContextId sets ContextId field to given value.

### HasContextId

`func (o *Variant) HasContextId() bool`

HasContextId returns a boolean if a field has been set.

### GetOverrideId

`func (o *Variant) GetOverrideId() string`

GetOverrideId returns the OverrideId field if non-nil, zero value otherwise.

### GetOverrideIdOk

`func (o *Variant) GetOverrideIdOk() (*string, bool)`

GetOverrideIdOk returns a tuple with the OverrideId field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOverrideId

`func (o *Variant) SetOverrideId(v string)`

SetOverrideId sets OverrideId field to given value.

### HasOverrideId

`func (o *Variant) HasOverrideId() bool`

HasOverrideId returns a boolean if a field has been set.

### GetOverrides

`func (o *Variant) GetOverrides() []interface{}`

GetOverrides returns the Overrides field if non-nil, zero value otherwise.

### GetOverridesOk

`func (o *Variant) GetOverridesOk() (*[]interface{}, bool)`

GetOverridesOk returns a tuple with the Overrides field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOverrides

`func (o *Variant) SetOverrides(v []interface{})`

SetOverrides sets Overrides field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


