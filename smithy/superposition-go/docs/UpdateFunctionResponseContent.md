# UpdateFunctionResponseContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**FunctionName** | **string** |  | 
**PublishedCode** | Pointer to **string** |  | [optional] 
**DraftCode** | **string** |  | 
**PublishedRuntimeVersion** | Pointer to **string** |  | [optional] 
**DraftRuntimeVersion** | **string** |  | 
**PublishedAt** | Pointer to **time.Time** |  | [optional] 
**DraftEditedAt** | **time.Time** |  | 
**PublishedBy** | Pointer to **string** |  | [optional] 
**DraftEditedBy** | **string** |  | 
**LastModifiedAt** | **time.Time** |  | 
**LastModifiedBy** | **string** |  | 
**ChangeReason** | **string** |  | 
**Description** | **string** |  | 

## Methods

### NewUpdateFunctionResponseContent

`func NewUpdateFunctionResponseContent(functionName string, draftCode string, draftRuntimeVersion string, draftEditedAt time.Time, draftEditedBy string, lastModifiedAt time.Time, lastModifiedBy string, changeReason string, description string, ) *UpdateFunctionResponseContent`

NewUpdateFunctionResponseContent instantiates a new UpdateFunctionResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewUpdateFunctionResponseContentWithDefaults

`func NewUpdateFunctionResponseContentWithDefaults() *UpdateFunctionResponseContent`

NewUpdateFunctionResponseContentWithDefaults instantiates a new UpdateFunctionResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetFunctionName

`func (o *UpdateFunctionResponseContent) GetFunctionName() string`

GetFunctionName returns the FunctionName field if non-nil, zero value otherwise.

### GetFunctionNameOk

`func (o *UpdateFunctionResponseContent) GetFunctionNameOk() (*string, bool)`

GetFunctionNameOk returns a tuple with the FunctionName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFunctionName

`func (o *UpdateFunctionResponseContent) SetFunctionName(v string)`

SetFunctionName sets FunctionName field to given value.


### GetPublishedCode

`func (o *UpdateFunctionResponseContent) GetPublishedCode() string`

GetPublishedCode returns the PublishedCode field if non-nil, zero value otherwise.

### GetPublishedCodeOk

`func (o *UpdateFunctionResponseContent) GetPublishedCodeOk() (*string, bool)`

GetPublishedCodeOk returns a tuple with the PublishedCode field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPublishedCode

`func (o *UpdateFunctionResponseContent) SetPublishedCode(v string)`

SetPublishedCode sets PublishedCode field to given value.

### HasPublishedCode

`func (o *UpdateFunctionResponseContent) HasPublishedCode() bool`

HasPublishedCode returns a boolean if a field has been set.

### GetDraftCode

`func (o *UpdateFunctionResponseContent) GetDraftCode() string`

GetDraftCode returns the DraftCode field if non-nil, zero value otherwise.

### GetDraftCodeOk

`func (o *UpdateFunctionResponseContent) GetDraftCodeOk() (*string, bool)`

GetDraftCodeOk returns a tuple with the DraftCode field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDraftCode

`func (o *UpdateFunctionResponseContent) SetDraftCode(v string)`

SetDraftCode sets DraftCode field to given value.


### GetPublishedRuntimeVersion

`func (o *UpdateFunctionResponseContent) GetPublishedRuntimeVersion() string`

GetPublishedRuntimeVersion returns the PublishedRuntimeVersion field if non-nil, zero value otherwise.

### GetPublishedRuntimeVersionOk

`func (o *UpdateFunctionResponseContent) GetPublishedRuntimeVersionOk() (*string, bool)`

GetPublishedRuntimeVersionOk returns a tuple with the PublishedRuntimeVersion field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPublishedRuntimeVersion

`func (o *UpdateFunctionResponseContent) SetPublishedRuntimeVersion(v string)`

SetPublishedRuntimeVersion sets PublishedRuntimeVersion field to given value.

### HasPublishedRuntimeVersion

`func (o *UpdateFunctionResponseContent) HasPublishedRuntimeVersion() bool`

HasPublishedRuntimeVersion returns a boolean if a field has been set.

### GetDraftRuntimeVersion

`func (o *UpdateFunctionResponseContent) GetDraftRuntimeVersion() string`

GetDraftRuntimeVersion returns the DraftRuntimeVersion field if non-nil, zero value otherwise.

### GetDraftRuntimeVersionOk

`func (o *UpdateFunctionResponseContent) GetDraftRuntimeVersionOk() (*string, bool)`

GetDraftRuntimeVersionOk returns a tuple with the DraftRuntimeVersion field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDraftRuntimeVersion

`func (o *UpdateFunctionResponseContent) SetDraftRuntimeVersion(v string)`

SetDraftRuntimeVersion sets DraftRuntimeVersion field to given value.


### GetPublishedAt

`func (o *UpdateFunctionResponseContent) GetPublishedAt() time.Time`

GetPublishedAt returns the PublishedAt field if non-nil, zero value otherwise.

### GetPublishedAtOk

`func (o *UpdateFunctionResponseContent) GetPublishedAtOk() (*time.Time, bool)`

GetPublishedAtOk returns a tuple with the PublishedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPublishedAt

`func (o *UpdateFunctionResponseContent) SetPublishedAt(v time.Time)`

SetPublishedAt sets PublishedAt field to given value.

### HasPublishedAt

`func (o *UpdateFunctionResponseContent) HasPublishedAt() bool`

HasPublishedAt returns a boolean if a field has been set.

### GetDraftEditedAt

`func (o *UpdateFunctionResponseContent) GetDraftEditedAt() time.Time`

GetDraftEditedAt returns the DraftEditedAt field if non-nil, zero value otherwise.

### GetDraftEditedAtOk

`func (o *UpdateFunctionResponseContent) GetDraftEditedAtOk() (*time.Time, bool)`

GetDraftEditedAtOk returns a tuple with the DraftEditedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDraftEditedAt

`func (o *UpdateFunctionResponseContent) SetDraftEditedAt(v time.Time)`

SetDraftEditedAt sets DraftEditedAt field to given value.


### GetPublishedBy

`func (o *UpdateFunctionResponseContent) GetPublishedBy() string`

GetPublishedBy returns the PublishedBy field if non-nil, zero value otherwise.

### GetPublishedByOk

`func (o *UpdateFunctionResponseContent) GetPublishedByOk() (*string, bool)`

GetPublishedByOk returns a tuple with the PublishedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPublishedBy

`func (o *UpdateFunctionResponseContent) SetPublishedBy(v string)`

SetPublishedBy sets PublishedBy field to given value.

### HasPublishedBy

`func (o *UpdateFunctionResponseContent) HasPublishedBy() bool`

HasPublishedBy returns a boolean if a field has been set.

### GetDraftEditedBy

`func (o *UpdateFunctionResponseContent) GetDraftEditedBy() string`

GetDraftEditedBy returns the DraftEditedBy field if non-nil, zero value otherwise.

### GetDraftEditedByOk

`func (o *UpdateFunctionResponseContent) GetDraftEditedByOk() (*string, bool)`

GetDraftEditedByOk returns a tuple with the DraftEditedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDraftEditedBy

`func (o *UpdateFunctionResponseContent) SetDraftEditedBy(v string)`

SetDraftEditedBy sets DraftEditedBy field to given value.


### GetLastModifiedAt

`func (o *UpdateFunctionResponseContent) GetLastModifiedAt() time.Time`

GetLastModifiedAt returns the LastModifiedAt field if non-nil, zero value otherwise.

### GetLastModifiedAtOk

`func (o *UpdateFunctionResponseContent) GetLastModifiedAtOk() (*time.Time, bool)`

GetLastModifiedAtOk returns a tuple with the LastModifiedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedAt

`func (o *UpdateFunctionResponseContent) SetLastModifiedAt(v time.Time)`

SetLastModifiedAt sets LastModifiedAt field to given value.


### GetLastModifiedBy

`func (o *UpdateFunctionResponseContent) GetLastModifiedBy() string`

GetLastModifiedBy returns the LastModifiedBy field if non-nil, zero value otherwise.

### GetLastModifiedByOk

`func (o *UpdateFunctionResponseContent) GetLastModifiedByOk() (*string, bool)`

GetLastModifiedByOk returns a tuple with the LastModifiedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedBy

`func (o *UpdateFunctionResponseContent) SetLastModifiedBy(v string)`

SetLastModifiedBy sets LastModifiedBy field to given value.


### GetChangeReason

`func (o *UpdateFunctionResponseContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *UpdateFunctionResponseContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *UpdateFunctionResponseContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.


### GetDescription

`func (o *UpdateFunctionResponseContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *UpdateFunctionResponseContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *UpdateFunctionResponseContent) SetDescription(v string)`

SetDescription sets Description field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


