# GetFunctionResponseContent

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

### NewGetFunctionResponseContent

`func NewGetFunctionResponseContent(functionName string, draftCode string, draftRuntimeVersion string, draftEditedAt time.Time, draftEditedBy string, lastModifiedAt time.Time, lastModifiedBy string, changeReason string, description string, ) *GetFunctionResponseContent`

NewGetFunctionResponseContent instantiates a new GetFunctionResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewGetFunctionResponseContentWithDefaults

`func NewGetFunctionResponseContentWithDefaults() *GetFunctionResponseContent`

NewGetFunctionResponseContentWithDefaults instantiates a new GetFunctionResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetFunctionName

`func (o *GetFunctionResponseContent) GetFunctionName() string`

GetFunctionName returns the FunctionName field if non-nil, zero value otherwise.

### GetFunctionNameOk

`func (o *GetFunctionResponseContent) GetFunctionNameOk() (*string, bool)`

GetFunctionNameOk returns a tuple with the FunctionName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFunctionName

`func (o *GetFunctionResponseContent) SetFunctionName(v string)`

SetFunctionName sets FunctionName field to given value.


### GetPublishedCode

`func (o *GetFunctionResponseContent) GetPublishedCode() string`

GetPublishedCode returns the PublishedCode field if non-nil, zero value otherwise.

### GetPublishedCodeOk

`func (o *GetFunctionResponseContent) GetPublishedCodeOk() (*string, bool)`

GetPublishedCodeOk returns a tuple with the PublishedCode field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPublishedCode

`func (o *GetFunctionResponseContent) SetPublishedCode(v string)`

SetPublishedCode sets PublishedCode field to given value.

### HasPublishedCode

`func (o *GetFunctionResponseContent) HasPublishedCode() bool`

HasPublishedCode returns a boolean if a field has been set.

### GetDraftCode

`func (o *GetFunctionResponseContent) GetDraftCode() string`

GetDraftCode returns the DraftCode field if non-nil, zero value otherwise.

### GetDraftCodeOk

`func (o *GetFunctionResponseContent) GetDraftCodeOk() (*string, bool)`

GetDraftCodeOk returns a tuple with the DraftCode field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDraftCode

`func (o *GetFunctionResponseContent) SetDraftCode(v string)`

SetDraftCode sets DraftCode field to given value.


### GetPublishedRuntimeVersion

`func (o *GetFunctionResponseContent) GetPublishedRuntimeVersion() string`

GetPublishedRuntimeVersion returns the PublishedRuntimeVersion field if non-nil, zero value otherwise.

### GetPublishedRuntimeVersionOk

`func (o *GetFunctionResponseContent) GetPublishedRuntimeVersionOk() (*string, bool)`

GetPublishedRuntimeVersionOk returns a tuple with the PublishedRuntimeVersion field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPublishedRuntimeVersion

`func (o *GetFunctionResponseContent) SetPublishedRuntimeVersion(v string)`

SetPublishedRuntimeVersion sets PublishedRuntimeVersion field to given value.

### HasPublishedRuntimeVersion

`func (o *GetFunctionResponseContent) HasPublishedRuntimeVersion() bool`

HasPublishedRuntimeVersion returns a boolean if a field has been set.

### GetDraftRuntimeVersion

`func (o *GetFunctionResponseContent) GetDraftRuntimeVersion() string`

GetDraftRuntimeVersion returns the DraftRuntimeVersion field if non-nil, zero value otherwise.

### GetDraftRuntimeVersionOk

`func (o *GetFunctionResponseContent) GetDraftRuntimeVersionOk() (*string, bool)`

GetDraftRuntimeVersionOk returns a tuple with the DraftRuntimeVersion field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDraftRuntimeVersion

`func (o *GetFunctionResponseContent) SetDraftRuntimeVersion(v string)`

SetDraftRuntimeVersion sets DraftRuntimeVersion field to given value.


### GetPublishedAt

`func (o *GetFunctionResponseContent) GetPublishedAt() time.Time`

GetPublishedAt returns the PublishedAt field if non-nil, zero value otherwise.

### GetPublishedAtOk

`func (o *GetFunctionResponseContent) GetPublishedAtOk() (*time.Time, bool)`

GetPublishedAtOk returns a tuple with the PublishedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPublishedAt

`func (o *GetFunctionResponseContent) SetPublishedAt(v time.Time)`

SetPublishedAt sets PublishedAt field to given value.

### HasPublishedAt

`func (o *GetFunctionResponseContent) HasPublishedAt() bool`

HasPublishedAt returns a boolean if a field has been set.

### GetDraftEditedAt

`func (o *GetFunctionResponseContent) GetDraftEditedAt() time.Time`

GetDraftEditedAt returns the DraftEditedAt field if non-nil, zero value otherwise.

### GetDraftEditedAtOk

`func (o *GetFunctionResponseContent) GetDraftEditedAtOk() (*time.Time, bool)`

GetDraftEditedAtOk returns a tuple with the DraftEditedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDraftEditedAt

`func (o *GetFunctionResponseContent) SetDraftEditedAt(v time.Time)`

SetDraftEditedAt sets DraftEditedAt field to given value.


### GetPublishedBy

`func (o *GetFunctionResponseContent) GetPublishedBy() string`

GetPublishedBy returns the PublishedBy field if non-nil, zero value otherwise.

### GetPublishedByOk

`func (o *GetFunctionResponseContent) GetPublishedByOk() (*string, bool)`

GetPublishedByOk returns a tuple with the PublishedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPublishedBy

`func (o *GetFunctionResponseContent) SetPublishedBy(v string)`

SetPublishedBy sets PublishedBy field to given value.

### HasPublishedBy

`func (o *GetFunctionResponseContent) HasPublishedBy() bool`

HasPublishedBy returns a boolean if a field has been set.

### GetDraftEditedBy

`func (o *GetFunctionResponseContent) GetDraftEditedBy() string`

GetDraftEditedBy returns the DraftEditedBy field if non-nil, zero value otherwise.

### GetDraftEditedByOk

`func (o *GetFunctionResponseContent) GetDraftEditedByOk() (*string, bool)`

GetDraftEditedByOk returns a tuple with the DraftEditedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDraftEditedBy

`func (o *GetFunctionResponseContent) SetDraftEditedBy(v string)`

SetDraftEditedBy sets DraftEditedBy field to given value.


### GetLastModifiedAt

`func (o *GetFunctionResponseContent) GetLastModifiedAt() time.Time`

GetLastModifiedAt returns the LastModifiedAt field if non-nil, zero value otherwise.

### GetLastModifiedAtOk

`func (o *GetFunctionResponseContent) GetLastModifiedAtOk() (*time.Time, bool)`

GetLastModifiedAtOk returns a tuple with the LastModifiedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedAt

`func (o *GetFunctionResponseContent) SetLastModifiedAt(v time.Time)`

SetLastModifiedAt sets LastModifiedAt field to given value.


### GetLastModifiedBy

`func (o *GetFunctionResponseContent) GetLastModifiedBy() string`

GetLastModifiedBy returns the LastModifiedBy field if non-nil, zero value otherwise.

### GetLastModifiedByOk

`func (o *GetFunctionResponseContent) GetLastModifiedByOk() (*string, bool)`

GetLastModifiedByOk returns a tuple with the LastModifiedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedBy

`func (o *GetFunctionResponseContent) SetLastModifiedBy(v string)`

SetLastModifiedBy sets LastModifiedBy field to given value.


### GetChangeReason

`func (o *GetFunctionResponseContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *GetFunctionResponseContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *GetFunctionResponseContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.


### GetDescription

`func (o *GetFunctionResponseContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *GetFunctionResponseContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *GetFunctionResponseContent) SetDescription(v string)`

SetDescription sets Description field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


