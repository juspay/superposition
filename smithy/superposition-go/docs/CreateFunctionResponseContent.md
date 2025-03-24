# CreateFunctionResponseContent

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

### NewCreateFunctionResponseContent

`func NewCreateFunctionResponseContent(functionName string, draftCode string, draftRuntimeVersion string, draftEditedAt time.Time, draftEditedBy string, lastModifiedAt time.Time, lastModifiedBy string, changeReason string, description string, ) *CreateFunctionResponseContent`

NewCreateFunctionResponseContent instantiates a new CreateFunctionResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewCreateFunctionResponseContentWithDefaults

`func NewCreateFunctionResponseContentWithDefaults() *CreateFunctionResponseContent`

NewCreateFunctionResponseContentWithDefaults instantiates a new CreateFunctionResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetFunctionName

`func (o *CreateFunctionResponseContent) GetFunctionName() string`

GetFunctionName returns the FunctionName field if non-nil, zero value otherwise.

### GetFunctionNameOk

`func (o *CreateFunctionResponseContent) GetFunctionNameOk() (*string, bool)`

GetFunctionNameOk returns a tuple with the FunctionName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFunctionName

`func (o *CreateFunctionResponseContent) SetFunctionName(v string)`

SetFunctionName sets FunctionName field to given value.


### GetPublishedCode

`func (o *CreateFunctionResponseContent) GetPublishedCode() string`

GetPublishedCode returns the PublishedCode field if non-nil, zero value otherwise.

### GetPublishedCodeOk

`func (o *CreateFunctionResponseContent) GetPublishedCodeOk() (*string, bool)`

GetPublishedCodeOk returns a tuple with the PublishedCode field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPublishedCode

`func (o *CreateFunctionResponseContent) SetPublishedCode(v string)`

SetPublishedCode sets PublishedCode field to given value.

### HasPublishedCode

`func (o *CreateFunctionResponseContent) HasPublishedCode() bool`

HasPublishedCode returns a boolean if a field has been set.

### GetDraftCode

`func (o *CreateFunctionResponseContent) GetDraftCode() string`

GetDraftCode returns the DraftCode field if non-nil, zero value otherwise.

### GetDraftCodeOk

`func (o *CreateFunctionResponseContent) GetDraftCodeOk() (*string, bool)`

GetDraftCodeOk returns a tuple with the DraftCode field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDraftCode

`func (o *CreateFunctionResponseContent) SetDraftCode(v string)`

SetDraftCode sets DraftCode field to given value.


### GetPublishedRuntimeVersion

`func (o *CreateFunctionResponseContent) GetPublishedRuntimeVersion() string`

GetPublishedRuntimeVersion returns the PublishedRuntimeVersion field if non-nil, zero value otherwise.

### GetPublishedRuntimeVersionOk

`func (o *CreateFunctionResponseContent) GetPublishedRuntimeVersionOk() (*string, bool)`

GetPublishedRuntimeVersionOk returns a tuple with the PublishedRuntimeVersion field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPublishedRuntimeVersion

`func (o *CreateFunctionResponseContent) SetPublishedRuntimeVersion(v string)`

SetPublishedRuntimeVersion sets PublishedRuntimeVersion field to given value.

### HasPublishedRuntimeVersion

`func (o *CreateFunctionResponseContent) HasPublishedRuntimeVersion() bool`

HasPublishedRuntimeVersion returns a boolean if a field has been set.

### GetDraftRuntimeVersion

`func (o *CreateFunctionResponseContent) GetDraftRuntimeVersion() string`

GetDraftRuntimeVersion returns the DraftRuntimeVersion field if non-nil, zero value otherwise.

### GetDraftRuntimeVersionOk

`func (o *CreateFunctionResponseContent) GetDraftRuntimeVersionOk() (*string, bool)`

GetDraftRuntimeVersionOk returns a tuple with the DraftRuntimeVersion field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDraftRuntimeVersion

`func (o *CreateFunctionResponseContent) SetDraftRuntimeVersion(v string)`

SetDraftRuntimeVersion sets DraftRuntimeVersion field to given value.


### GetPublishedAt

`func (o *CreateFunctionResponseContent) GetPublishedAt() time.Time`

GetPublishedAt returns the PublishedAt field if non-nil, zero value otherwise.

### GetPublishedAtOk

`func (o *CreateFunctionResponseContent) GetPublishedAtOk() (*time.Time, bool)`

GetPublishedAtOk returns a tuple with the PublishedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPublishedAt

`func (o *CreateFunctionResponseContent) SetPublishedAt(v time.Time)`

SetPublishedAt sets PublishedAt field to given value.

### HasPublishedAt

`func (o *CreateFunctionResponseContent) HasPublishedAt() bool`

HasPublishedAt returns a boolean if a field has been set.

### GetDraftEditedAt

`func (o *CreateFunctionResponseContent) GetDraftEditedAt() time.Time`

GetDraftEditedAt returns the DraftEditedAt field if non-nil, zero value otherwise.

### GetDraftEditedAtOk

`func (o *CreateFunctionResponseContent) GetDraftEditedAtOk() (*time.Time, bool)`

GetDraftEditedAtOk returns a tuple with the DraftEditedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDraftEditedAt

`func (o *CreateFunctionResponseContent) SetDraftEditedAt(v time.Time)`

SetDraftEditedAt sets DraftEditedAt field to given value.


### GetPublishedBy

`func (o *CreateFunctionResponseContent) GetPublishedBy() string`

GetPublishedBy returns the PublishedBy field if non-nil, zero value otherwise.

### GetPublishedByOk

`func (o *CreateFunctionResponseContent) GetPublishedByOk() (*string, bool)`

GetPublishedByOk returns a tuple with the PublishedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPublishedBy

`func (o *CreateFunctionResponseContent) SetPublishedBy(v string)`

SetPublishedBy sets PublishedBy field to given value.

### HasPublishedBy

`func (o *CreateFunctionResponseContent) HasPublishedBy() bool`

HasPublishedBy returns a boolean if a field has been set.

### GetDraftEditedBy

`func (o *CreateFunctionResponseContent) GetDraftEditedBy() string`

GetDraftEditedBy returns the DraftEditedBy field if non-nil, zero value otherwise.

### GetDraftEditedByOk

`func (o *CreateFunctionResponseContent) GetDraftEditedByOk() (*string, bool)`

GetDraftEditedByOk returns a tuple with the DraftEditedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDraftEditedBy

`func (o *CreateFunctionResponseContent) SetDraftEditedBy(v string)`

SetDraftEditedBy sets DraftEditedBy field to given value.


### GetLastModifiedAt

`func (o *CreateFunctionResponseContent) GetLastModifiedAt() time.Time`

GetLastModifiedAt returns the LastModifiedAt field if non-nil, zero value otherwise.

### GetLastModifiedAtOk

`func (o *CreateFunctionResponseContent) GetLastModifiedAtOk() (*time.Time, bool)`

GetLastModifiedAtOk returns a tuple with the LastModifiedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedAt

`func (o *CreateFunctionResponseContent) SetLastModifiedAt(v time.Time)`

SetLastModifiedAt sets LastModifiedAt field to given value.


### GetLastModifiedBy

`func (o *CreateFunctionResponseContent) GetLastModifiedBy() string`

GetLastModifiedBy returns the LastModifiedBy field if non-nil, zero value otherwise.

### GetLastModifiedByOk

`func (o *CreateFunctionResponseContent) GetLastModifiedByOk() (*string, bool)`

GetLastModifiedByOk returns a tuple with the LastModifiedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedBy

`func (o *CreateFunctionResponseContent) SetLastModifiedBy(v string)`

SetLastModifiedBy sets LastModifiedBy field to given value.


### GetChangeReason

`func (o *CreateFunctionResponseContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *CreateFunctionResponseContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *CreateFunctionResponseContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.


### GetDescription

`func (o *CreateFunctionResponseContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *CreateFunctionResponseContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *CreateFunctionResponseContent) SetDescription(v string)`

SetDescription sets Description field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


