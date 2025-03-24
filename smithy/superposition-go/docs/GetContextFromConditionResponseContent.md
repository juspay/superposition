# GetContextFromConditionResponseContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Id** | **string** |  | 
**Value** | Pointer to **map[string]interface{}** |  | [optional] 
**Override** | Pointer to **map[string]interface{}** |  | [optional] 
**OverrideId** | Pointer to **string** |  | [optional] 
**Weight** | Pointer to **float32** |  | [optional] 
**OverrideWithKeys** | Pointer to **[]string** |  | [optional] 
**Description** | Pointer to **string** |  | [optional] 
**ChangeReason** | Pointer to **string** |  | [optional] 
**CreatedAt** | Pointer to **time.Time** |  | [optional] 
**CreatedBy** | Pointer to **string** |  | [optional] 
**LastModifiedAt** | Pointer to **time.Time** |  | [optional] 
**LastModifiedBy** | Pointer to **string** |  | [optional] 

## Methods

### NewGetContextFromConditionResponseContent

`func NewGetContextFromConditionResponseContent(id string, ) *GetContextFromConditionResponseContent`

NewGetContextFromConditionResponseContent instantiates a new GetContextFromConditionResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewGetContextFromConditionResponseContentWithDefaults

`func NewGetContextFromConditionResponseContentWithDefaults() *GetContextFromConditionResponseContent`

NewGetContextFromConditionResponseContentWithDefaults instantiates a new GetContextFromConditionResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetId

`func (o *GetContextFromConditionResponseContent) GetId() string`

GetId returns the Id field if non-nil, zero value otherwise.

### GetIdOk

`func (o *GetContextFromConditionResponseContent) GetIdOk() (*string, bool)`

GetIdOk returns a tuple with the Id field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetId

`func (o *GetContextFromConditionResponseContent) SetId(v string)`

SetId sets Id field to given value.


### GetValue

`func (o *GetContextFromConditionResponseContent) GetValue() map[string]interface{}`

GetValue returns the Value field if non-nil, zero value otherwise.

### GetValueOk

`func (o *GetContextFromConditionResponseContent) GetValueOk() (*map[string]interface{}, bool)`

GetValueOk returns a tuple with the Value field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetValue

`func (o *GetContextFromConditionResponseContent) SetValue(v map[string]interface{})`

SetValue sets Value field to given value.

### HasValue

`func (o *GetContextFromConditionResponseContent) HasValue() bool`

HasValue returns a boolean if a field has been set.

### GetOverride

`func (o *GetContextFromConditionResponseContent) GetOverride() map[string]interface{}`

GetOverride returns the Override field if non-nil, zero value otherwise.

### GetOverrideOk

`func (o *GetContextFromConditionResponseContent) GetOverrideOk() (*map[string]interface{}, bool)`

GetOverrideOk returns a tuple with the Override field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOverride

`func (o *GetContextFromConditionResponseContent) SetOverride(v map[string]interface{})`

SetOverride sets Override field to given value.

### HasOverride

`func (o *GetContextFromConditionResponseContent) HasOverride() bool`

HasOverride returns a boolean if a field has been set.

### GetOverrideId

`func (o *GetContextFromConditionResponseContent) GetOverrideId() string`

GetOverrideId returns the OverrideId field if non-nil, zero value otherwise.

### GetOverrideIdOk

`func (o *GetContextFromConditionResponseContent) GetOverrideIdOk() (*string, bool)`

GetOverrideIdOk returns a tuple with the OverrideId field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOverrideId

`func (o *GetContextFromConditionResponseContent) SetOverrideId(v string)`

SetOverrideId sets OverrideId field to given value.

### HasOverrideId

`func (o *GetContextFromConditionResponseContent) HasOverrideId() bool`

HasOverrideId returns a boolean if a field has been set.

### GetWeight

`func (o *GetContextFromConditionResponseContent) GetWeight() float32`

GetWeight returns the Weight field if non-nil, zero value otherwise.

### GetWeightOk

`func (o *GetContextFromConditionResponseContent) GetWeightOk() (*float32, bool)`

GetWeightOk returns a tuple with the Weight field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetWeight

`func (o *GetContextFromConditionResponseContent) SetWeight(v float32)`

SetWeight sets Weight field to given value.

### HasWeight

`func (o *GetContextFromConditionResponseContent) HasWeight() bool`

HasWeight returns a boolean if a field has been set.

### GetOverrideWithKeys

`func (o *GetContextFromConditionResponseContent) GetOverrideWithKeys() []string`

GetOverrideWithKeys returns the OverrideWithKeys field if non-nil, zero value otherwise.

### GetOverrideWithKeysOk

`func (o *GetContextFromConditionResponseContent) GetOverrideWithKeysOk() (*[]string, bool)`

GetOverrideWithKeysOk returns a tuple with the OverrideWithKeys field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOverrideWithKeys

`func (o *GetContextFromConditionResponseContent) SetOverrideWithKeys(v []string)`

SetOverrideWithKeys sets OverrideWithKeys field to given value.

### HasOverrideWithKeys

`func (o *GetContextFromConditionResponseContent) HasOverrideWithKeys() bool`

HasOverrideWithKeys returns a boolean if a field has been set.

### GetDescription

`func (o *GetContextFromConditionResponseContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *GetContextFromConditionResponseContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *GetContextFromConditionResponseContent) SetDescription(v string)`

SetDescription sets Description field to given value.

### HasDescription

`func (o *GetContextFromConditionResponseContent) HasDescription() bool`

HasDescription returns a boolean if a field has been set.

### GetChangeReason

`func (o *GetContextFromConditionResponseContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *GetContextFromConditionResponseContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *GetContextFromConditionResponseContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.

### HasChangeReason

`func (o *GetContextFromConditionResponseContent) HasChangeReason() bool`

HasChangeReason returns a boolean if a field has been set.

### GetCreatedAt

`func (o *GetContextFromConditionResponseContent) GetCreatedAt() time.Time`

GetCreatedAt returns the CreatedAt field if non-nil, zero value otherwise.

### GetCreatedAtOk

`func (o *GetContextFromConditionResponseContent) GetCreatedAtOk() (*time.Time, bool)`

GetCreatedAtOk returns a tuple with the CreatedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedAt

`func (o *GetContextFromConditionResponseContent) SetCreatedAt(v time.Time)`

SetCreatedAt sets CreatedAt field to given value.

### HasCreatedAt

`func (o *GetContextFromConditionResponseContent) HasCreatedAt() bool`

HasCreatedAt returns a boolean if a field has been set.

### GetCreatedBy

`func (o *GetContextFromConditionResponseContent) GetCreatedBy() string`

GetCreatedBy returns the CreatedBy field if non-nil, zero value otherwise.

### GetCreatedByOk

`func (o *GetContextFromConditionResponseContent) GetCreatedByOk() (*string, bool)`

GetCreatedByOk returns a tuple with the CreatedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedBy

`func (o *GetContextFromConditionResponseContent) SetCreatedBy(v string)`

SetCreatedBy sets CreatedBy field to given value.

### HasCreatedBy

`func (o *GetContextFromConditionResponseContent) HasCreatedBy() bool`

HasCreatedBy returns a boolean if a field has been set.

### GetLastModifiedAt

`func (o *GetContextFromConditionResponseContent) GetLastModifiedAt() time.Time`

GetLastModifiedAt returns the LastModifiedAt field if non-nil, zero value otherwise.

### GetLastModifiedAtOk

`func (o *GetContextFromConditionResponseContent) GetLastModifiedAtOk() (*time.Time, bool)`

GetLastModifiedAtOk returns a tuple with the LastModifiedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedAt

`func (o *GetContextFromConditionResponseContent) SetLastModifiedAt(v time.Time)`

SetLastModifiedAt sets LastModifiedAt field to given value.

### HasLastModifiedAt

`func (o *GetContextFromConditionResponseContent) HasLastModifiedAt() bool`

HasLastModifiedAt returns a boolean if a field has been set.

### GetLastModifiedBy

`func (o *GetContextFromConditionResponseContent) GetLastModifiedBy() string`

GetLastModifiedBy returns the LastModifiedBy field if non-nil, zero value otherwise.

### GetLastModifiedByOk

`func (o *GetContextFromConditionResponseContent) GetLastModifiedByOk() (*string, bool)`

GetLastModifiedByOk returns a tuple with the LastModifiedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedBy

`func (o *GetContextFromConditionResponseContent) SetLastModifiedBy(v string)`

SetLastModifiedBy sets LastModifiedBy field to given value.

### HasLastModifiedBy

`func (o *GetContextFromConditionResponseContent) HasLastModifiedBy() bool`

HasLastModifiedBy returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


