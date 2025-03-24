# ConcludeExperimentResponseContent

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

### NewConcludeExperimentResponseContent

`func NewConcludeExperimentResponseContent(id string, createdAt time.Time, createdBy string, lastModified time.Time, name string, overrideKeys []string, status ExperimentStatusType, trafficPercentage float32, context map[string]interface{}, variants []Variant, lastModifiedBy string, chosenVariant string, description string, changeReason string, ) *ConcludeExperimentResponseContent`

NewConcludeExperimentResponseContent instantiates a new ConcludeExperimentResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewConcludeExperimentResponseContentWithDefaults

`func NewConcludeExperimentResponseContentWithDefaults() *ConcludeExperimentResponseContent`

NewConcludeExperimentResponseContentWithDefaults instantiates a new ConcludeExperimentResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetId

`func (o *ConcludeExperimentResponseContent) GetId() string`

GetId returns the Id field if non-nil, zero value otherwise.

### GetIdOk

`func (o *ConcludeExperimentResponseContent) GetIdOk() (*string, bool)`

GetIdOk returns a tuple with the Id field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetId

`func (o *ConcludeExperimentResponseContent) SetId(v string)`

SetId sets Id field to given value.


### GetCreatedAt

`func (o *ConcludeExperimentResponseContent) GetCreatedAt() time.Time`

GetCreatedAt returns the CreatedAt field if non-nil, zero value otherwise.

### GetCreatedAtOk

`func (o *ConcludeExperimentResponseContent) GetCreatedAtOk() (*time.Time, bool)`

GetCreatedAtOk returns a tuple with the CreatedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedAt

`func (o *ConcludeExperimentResponseContent) SetCreatedAt(v time.Time)`

SetCreatedAt sets CreatedAt field to given value.


### GetCreatedBy

`func (o *ConcludeExperimentResponseContent) GetCreatedBy() string`

GetCreatedBy returns the CreatedBy field if non-nil, zero value otherwise.

### GetCreatedByOk

`func (o *ConcludeExperimentResponseContent) GetCreatedByOk() (*string, bool)`

GetCreatedByOk returns a tuple with the CreatedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedBy

`func (o *ConcludeExperimentResponseContent) SetCreatedBy(v string)`

SetCreatedBy sets CreatedBy field to given value.


### GetLastModified

`func (o *ConcludeExperimentResponseContent) GetLastModified() time.Time`

GetLastModified returns the LastModified field if non-nil, zero value otherwise.

### GetLastModifiedOk

`func (o *ConcludeExperimentResponseContent) GetLastModifiedOk() (*time.Time, bool)`

GetLastModifiedOk returns a tuple with the LastModified field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModified

`func (o *ConcludeExperimentResponseContent) SetLastModified(v time.Time)`

SetLastModified sets LastModified field to given value.


### GetName

`func (o *ConcludeExperimentResponseContent) GetName() string`

GetName returns the Name field if non-nil, zero value otherwise.

### GetNameOk

`func (o *ConcludeExperimentResponseContent) GetNameOk() (*string, bool)`

GetNameOk returns a tuple with the Name field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetName

`func (o *ConcludeExperimentResponseContent) SetName(v string)`

SetName sets Name field to given value.


### GetOverrideKeys

`func (o *ConcludeExperimentResponseContent) GetOverrideKeys() []string`

GetOverrideKeys returns the OverrideKeys field if non-nil, zero value otherwise.

### GetOverrideKeysOk

`func (o *ConcludeExperimentResponseContent) GetOverrideKeysOk() (*[]string, bool)`

GetOverrideKeysOk returns a tuple with the OverrideKeys field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOverrideKeys

`func (o *ConcludeExperimentResponseContent) SetOverrideKeys(v []string)`

SetOverrideKeys sets OverrideKeys field to given value.


### GetStatus

`func (o *ConcludeExperimentResponseContent) GetStatus() ExperimentStatusType`

GetStatus returns the Status field if non-nil, zero value otherwise.

### GetStatusOk

`func (o *ConcludeExperimentResponseContent) GetStatusOk() (*ExperimentStatusType, bool)`

GetStatusOk returns a tuple with the Status field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetStatus

`func (o *ConcludeExperimentResponseContent) SetStatus(v ExperimentStatusType)`

SetStatus sets Status field to given value.


### GetTrafficPercentage

`func (o *ConcludeExperimentResponseContent) GetTrafficPercentage() float32`

GetTrafficPercentage returns the TrafficPercentage field if non-nil, zero value otherwise.

### GetTrafficPercentageOk

`func (o *ConcludeExperimentResponseContent) GetTrafficPercentageOk() (*float32, bool)`

GetTrafficPercentageOk returns a tuple with the TrafficPercentage field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTrafficPercentage

`func (o *ConcludeExperimentResponseContent) SetTrafficPercentage(v float32)`

SetTrafficPercentage sets TrafficPercentage field to given value.


### GetContext

`func (o *ConcludeExperimentResponseContent) GetContext() map[string]interface{}`

GetContext returns the Context field if non-nil, zero value otherwise.

### GetContextOk

`func (o *ConcludeExperimentResponseContent) GetContextOk() (*map[string]interface{}, bool)`

GetContextOk returns a tuple with the Context field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetContext

`func (o *ConcludeExperimentResponseContent) SetContext(v map[string]interface{})`

SetContext sets Context field to given value.


### GetVariants

`func (o *ConcludeExperimentResponseContent) GetVariants() []Variant`

GetVariants returns the Variants field if non-nil, zero value otherwise.

### GetVariantsOk

`func (o *ConcludeExperimentResponseContent) GetVariantsOk() (*[]Variant, bool)`

GetVariantsOk returns a tuple with the Variants field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetVariants

`func (o *ConcludeExperimentResponseContent) SetVariants(v []Variant)`

SetVariants sets Variants field to given value.


### GetLastModifiedBy

`func (o *ConcludeExperimentResponseContent) GetLastModifiedBy() string`

GetLastModifiedBy returns the LastModifiedBy field if non-nil, zero value otherwise.

### GetLastModifiedByOk

`func (o *ConcludeExperimentResponseContent) GetLastModifiedByOk() (*string, bool)`

GetLastModifiedByOk returns a tuple with the LastModifiedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedBy

`func (o *ConcludeExperimentResponseContent) SetLastModifiedBy(v string)`

SetLastModifiedBy sets LastModifiedBy field to given value.


### GetChosenVariant

`func (o *ConcludeExperimentResponseContent) GetChosenVariant() string`

GetChosenVariant returns the ChosenVariant field if non-nil, zero value otherwise.

### GetChosenVariantOk

`func (o *ConcludeExperimentResponseContent) GetChosenVariantOk() (*string, bool)`

GetChosenVariantOk returns a tuple with the ChosenVariant field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChosenVariant

`func (o *ConcludeExperimentResponseContent) SetChosenVariant(v string)`

SetChosenVariant sets ChosenVariant field to given value.


### GetDescription

`func (o *ConcludeExperimentResponseContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *ConcludeExperimentResponseContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *ConcludeExperimentResponseContent) SetDescription(v string)`

SetDescription sets Description field to given value.


### GetChangeReason

`func (o *ConcludeExperimentResponseContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *ConcludeExperimentResponseContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *ConcludeExperimentResponseContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


