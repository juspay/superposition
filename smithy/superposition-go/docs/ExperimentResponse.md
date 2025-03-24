# ExperimentResponse

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

### NewExperimentResponse

`func NewExperimentResponse(id string, createdAt time.Time, createdBy string, lastModified time.Time, name string, overrideKeys []string, status ExperimentStatusType, trafficPercentage float32, context map[string]interface{}, variants []Variant, lastModifiedBy string, chosenVariant string, description string, changeReason string, ) *ExperimentResponse`

NewExperimentResponse instantiates a new ExperimentResponse object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewExperimentResponseWithDefaults

`func NewExperimentResponseWithDefaults() *ExperimentResponse`

NewExperimentResponseWithDefaults instantiates a new ExperimentResponse object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetId

`func (o *ExperimentResponse) GetId() string`

GetId returns the Id field if non-nil, zero value otherwise.

### GetIdOk

`func (o *ExperimentResponse) GetIdOk() (*string, bool)`

GetIdOk returns a tuple with the Id field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetId

`func (o *ExperimentResponse) SetId(v string)`

SetId sets Id field to given value.


### GetCreatedAt

`func (o *ExperimentResponse) GetCreatedAt() time.Time`

GetCreatedAt returns the CreatedAt field if non-nil, zero value otherwise.

### GetCreatedAtOk

`func (o *ExperimentResponse) GetCreatedAtOk() (*time.Time, bool)`

GetCreatedAtOk returns a tuple with the CreatedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedAt

`func (o *ExperimentResponse) SetCreatedAt(v time.Time)`

SetCreatedAt sets CreatedAt field to given value.


### GetCreatedBy

`func (o *ExperimentResponse) GetCreatedBy() string`

GetCreatedBy returns the CreatedBy field if non-nil, zero value otherwise.

### GetCreatedByOk

`func (o *ExperimentResponse) GetCreatedByOk() (*string, bool)`

GetCreatedByOk returns a tuple with the CreatedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedBy

`func (o *ExperimentResponse) SetCreatedBy(v string)`

SetCreatedBy sets CreatedBy field to given value.


### GetLastModified

`func (o *ExperimentResponse) GetLastModified() time.Time`

GetLastModified returns the LastModified field if non-nil, zero value otherwise.

### GetLastModifiedOk

`func (o *ExperimentResponse) GetLastModifiedOk() (*time.Time, bool)`

GetLastModifiedOk returns a tuple with the LastModified field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModified

`func (o *ExperimentResponse) SetLastModified(v time.Time)`

SetLastModified sets LastModified field to given value.


### GetName

`func (o *ExperimentResponse) GetName() string`

GetName returns the Name field if non-nil, zero value otherwise.

### GetNameOk

`func (o *ExperimentResponse) GetNameOk() (*string, bool)`

GetNameOk returns a tuple with the Name field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetName

`func (o *ExperimentResponse) SetName(v string)`

SetName sets Name field to given value.


### GetOverrideKeys

`func (o *ExperimentResponse) GetOverrideKeys() []string`

GetOverrideKeys returns the OverrideKeys field if non-nil, zero value otherwise.

### GetOverrideKeysOk

`func (o *ExperimentResponse) GetOverrideKeysOk() (*[]string, bool)`

GetOverrideKeysOk returns a tuple with the OverrideKeys field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOverrideKeys

`func (o *ExperimentResponse) SetOverrideKeys(v []string)`

SetOverrideKeys sets OverrideKeys field to given value.


### GetStatus

`func (o *ExperimentResponse) GetStatus() ExperimentStatusType`

GetStatus returns the Status field if non-nil, zero value otherwise.

### GetStatusOk

`func (o *ExperimentResponse) GetStatusOk() (*ExperimentStatusType, bool)`

GetStatusOk returns a tuple with the Status field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetStatus

`func (o *ExperimentResponse) SetStatus(v ExperimentStatusType)`

SetStatus sets Status field to given value.


### GetTrafficPercentage

`func (o *ExperimentResponse) GetTrafficPercentage() float32`

GetTrafficPercentage returns the TrafficPercentage field if non-nil, zero value otherwise.

### GetTrafficPercentageOk

`func (o *ExperimentResponse) GetTrafficPercentageOk() (*float32, bool)`

GetTrafficPercentageOk returns a tuple with the TrafficPercentage field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTrafficPercentage

`func (o *ExperimentResponse) SetTrafficPercentage(v float32)`

SetTrafficPercentage sets TrafficPercentage field to given value.


### GetContext

`func (o *ExperimentResponse) GetContext() map[string]interface{}`

GetContext returns the Context field if non-nil, zero value otherwise.

### GetContextOk

`func (o *ExperimentResponse) GetContextOk() (*map[string]interface{}, bool)`

GetContextOk returns a tuple with the Context field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetContext

`func (o *ExperimentResponse) SetContext(v map[string]interface{})`

SetContext sets Context field to given value.


### GetVariants

`func (o *ExperimentResponse) GetVariants() []Variant`

GetVariants returns the Variants field if non-nil, zero value otherwise.

### GetVariantsOk

`func (o *ExperimentResponse) GetVariantsOk() (*[]Variant, bool)`

GetVariantsOk returns a tuple with the Variants field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetVariants

`func (o *ExperimentResponse) SetVariants(v []Variant)`

SetVariants sets Variants field to given value.


### GetLastModifiedBy

`func (o *ExperimentResponse) GetLastModifiedBy() string`

GetLastModifiedBy returns the LastModifiedBy field if non-nil, zero value otherwise.

### GetLastModifiedByOk

`func (o *ExperimentResponse) GetLastModifiedByOk() (*string, bool)`

GetLastModifiedByOk returns a tuple with the LastModifiedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedBy

`func (o *ExperimentResponse) SetLastModifiedBy(v string)`

SetLastModifiedBy sets LastModifiedBy field to given value.


### GetChosenVariant

`func (o *ExperimentResponse) GetChosenVariant() string`

GetChosenVariant returns the ChosenVariant field if non-nil, zero value otherwise.

### GetChosenVariantOk

`func (o *ExperimentResponse) GetChosenVariantOk() (*string, bool)`

GetChosenVariantOk returns a tuple with the ChosenVariant field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChosenVariant

`func (o *ExperimentResponse) SetChosenVariant(v string)`

SetChosenVariant sets ChosenVariant field to given value.


### GetDescription

`func (o *ExperimentResponse) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *ExperimentResponse) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *ExperimentResponse) SetDescription(v string)`

SetDescription sets Description field to given value.


### GetChangeReason

`func (o *ExperimentResponse) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *ExperimentResponse) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *ExperimentResponse) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


