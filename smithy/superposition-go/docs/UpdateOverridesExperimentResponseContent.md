# UpdateOverridesExperimentResponseContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Id** | **string** |  | 
**CreatedAt** | **time.Time** |  | 
**CreatedBy** | **string** |  | 
**LastModified** | **time.Time** |  | 
**Name** | **string** |  | 
**OverrideKeys** | **[]string** |  | 
**Status** | [**ExperimentStatusType**](ExperimentStatusType.md) |  | 
**TrafficPercentage** | **float32** |  | 
**Context** | **map[string]interface{}** |  | 
**Variants** | [**[]Variant**](Variant.md) |  | 
**LastModifiedBy** | **string** |  | 
**ChosenVariant** | **string** |  | 
**Description** | **string** |  | 
**ChangeReason** | **string** |  | 

## Methods

### NewUpdateOverridesExperimentResponseContent

`func NewUpdateOverridesExperimentResponseContent(id string, createdAt time.Time, createdBy string, lastModified time.Time, name string, overrideKeys []string, status ExperimentStatusType, trafficPercentage float32, context map[string]interface{}, variants []Variant, lastModifiedBy string, chosenVariant string, description string, changeReason string, ) *UpdateOverridesExperimentResponseContent`

NewUpdateOverridesExperimentResponseContent instantiates a new UpdateOverridesExperimentResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewUpdateOverridesExperimentResponseContentWithDefaults

`func NewUpdateOverridesExperimentResponseContentWithDefaults() *UpdateOverridesExperimentResponseContent`

NewUpdateOverridesExperimentResponseContentWithDefaults instantiates a new UpdateOverridesExperimentResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetId

`func (o *UpdateOverridesExperimentResponseContent) GetId() string`

GetId returns the Id field if non-nil, zero value otherwise.

### GetIdOk

`func (o *UpdateOverridesExperimentResponseContent) GetIdOk() (*string, bool)`

GetIdOk returns a tuple with the Id field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetId

`func (o *UpdateOverridesExperimentResponseContent) SetId(v string)`

SetId sets Id field to given value.


### GetCreatedAt

`func (o *UpdateOverridesExperimentResponseContent) GetCreatedAt() time.Time`

GetCreatedAt returns the CreatedAt field if non-nil, zero value otherwise.

### GetCreatedAtOk

`func (o *UpdateOverridesExperimentResponseContent) GetCreatedAtOk() (*time.Time, bool)`

GetCreatedAtOk returns a tuple with the CreatedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedAt

`func (o *UpdateOverridesExperimentResponseContent) SetCreatedAt(v time.Time)`

SetCreatedAt sets CreatedAt field to given value.


### GetCreatedBy

`func (o *UpdateOverridesExperimentResponseContent) GetCreatedBy() string`

GetCreatedBy returns the CreatedBy field if non-nil, zero value otherwise.

### GetCreatedByOk

`func (o *UpdateOverridesExperimentResponseContent) GetCreatedByOk() (*string, bool)`

GetCreatedByOk returns a tuple with the CreatedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedBy

`func (o *UpdateOverridesExperimentResponseContent) SetCreatedBy(v string)`

SetCreatedBy sets CreatedBy field to given value.


### GetLastModified

`func (o *UpdateOverridesExperimentResponseContent) GetLastModified() time.Time`

GetLastModified returns the LastModified field if non-nil, zero value otherwise.

### GetLastModifiedOk

`func (o *UpdateOverridesExperimentResponseContent) GetLastModifiedOk() (*time.Time, bool)`

GetLastModifiedOk returns a tuple with the LastModified field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModified

`func (o *UpdateOverridesExperimentResponseContent) SetLastModified(v time.Time)`

SetLastModified sets LastModified field to given value.


### GetName

`func (o *UpdateOverridesExperimentResponseContent) GetName() string`

GetName returns the Name field if non-nil, zero value otherwise.

### GetNameOk

`func (o *UpdateOverridesExperimentResponseContent) GetNameOk() (*string, bool)`

GetNameOk returns a tuple with the Name field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetName

`func (o *UpdateOverridesExperimentResponseContent) SetName(v string)`

SetName sets Name field to given value.


### GetOverrideKeys

`func (o *UpdateOverridesExperimentResponseContent) GetOverrideKeys() []string`

GetOverrideKeys returns the OverrideKeys field if non-nil, zero value otherwise.

### GetOverrideKeysOk

`func (o *UpdateOverridesExperimentResponseContent) GetOverrideKeysOk() (*[]string, bool)`

GetOverrideKeysOk returns a tuple with the OverrideKeys field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOverrideKeys

`func (o *UpdateOverridesExperimentResponseContent) SetOverrideKeys(v []string)`

SetOverrideKeys sets OverrideKeys field to given value.


### GetStatus

`func (o *UpdateOverridesExperimentResponseContent) GetStatus() ExperimentStatusType`

GetStatus returns the Status field if non-nil, zero value otherwise.

### GetStatusOk

`func (o *UpdateOverridesExperimentResponseContent) GetStatusOk() (*ExperimentStatusType, bool)`

GetStatusOk returns a tuple with the Status field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetStatus

`func (o *UpdateOverridesExperimentResponseContent) SetStatus(v ExperimentStatusType)`

SetStatus sets Status field to given value.


### GetTrafficPercentage

`func (o *UpdateOverridesExperimentResponseContent) GetTrafficPercentage() float32`

GetTrafficPercentage returns the TrafficPercentage field if non-nil, zero value otherwise.

### GetTrafficPercentageOk

`func (o *UpdateOverridesExperimentResponseContent) GetTrafficPercentageOk() (*float32, bool)`

GetTrafficPercentageOk returns a tuple with the TrafficPercentage field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTrafficPercentage

`func (o *UpdateOverridesExperimentResponseContent) SetTrafficPercentage(v float32)`

SetTrafficPercentage sets TrafficPercentage field to given value.


### GetContext

`func (o *UpdateOverridesExperimentResponseContent) GetContext() map[string]interface{}`

GetContext returns the Context field if non-nil, zero value otherwise.

### GetContextOk

`func (o *UpdateOverridesExperimentResponseContent) GetContextOk() (*map[string]interface{}, bool)`

GetContextOk returns a tuple with the Context field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetContext

`func (o *UpdateOverridesExperimentResponseContent) SetContext(v map[string]interface{})`

SetContext sets Context field to given value.


### GetVariants

`func (o *UpdateOverridesExperimentResponseContent) GetVariants() []Variant`

GetVariants returns the Variants field if non-nil, zero value otherwise.

### GetVariantsOk

`func (o *UpdateOverridesExperimentResponseContent) GetVariantsOk() (*[]Variant, bool)`

GetVariantsOk returns a tuple with the Variants field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetVariants

`func (o *UpdateOverridesExperimentResponseContent) SetVariants(v []Variant)`

SetVariants sets Variants field to given value.


### GetLastModifiedBy

`func (o *UpdateOverridesExperimentResponseContent) GetLastModifiedBy() string`

GetLastModifiedBy returns the LastModifiedBy field if non-nil, zero value otherwise.

### GetLastModifiedByOk

`func (o *UpdateOverridesExperimentResponseContent) GetLastModifiedByOk() (*string, bool)`

GetLastModifiedByOk returns a tuple with the LastModifiedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedBy

`func (o *UpdateOverridesExperimentResponseContent) SetLastModifiedBy(v string)`

SetLastModifiedBy sets LastModifiedBy field to given value.


### GetChosenVariant

`func (o *UpdateOverridesExperimentResponseContent) GetChosenVariant() string`

GetChosenVariant returns the ChosenVariant field if non-nil, zero value otherwise.

### GetChosenVariantOk

`func (o *UpdateOverridesExperimentResponseContent) GetChosenVariantOk() (*string, bool)`

GetChosenVariantOk returns a tuple with the ChosenVariant field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChosenVariant

`func (o *UpdateOverridesExperimentResponseContent) SetChosenVariant(v string)`

SetChosenVariant sets ChosenVariant field to given value.


### GetDescription

`func (o *UpdateOverridesExperimentResponseContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *UpdateOverridesExperimentResponseContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *UpdateOverridesExperimentResponseContent) SetDescription(v string)`

SetDescription sets Description field to given value.


### GetChangeReason

`func (o *UpdateOverridesExperimentResponseContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *UpdateOverridesExperimentResponseContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *UpdateOverridesExperimentResponseContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


