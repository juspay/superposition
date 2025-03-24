# PublishResponseContent

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

### NewPublishResponseContent

`func NewPublishResponseContent(functionName string, draftCode string, draftRuntimeVersion string, draftEditedAt time.Time, draftEditedBy string, lastModifiedAt time.Time, lastModifiedBy string, changeReason string, description string, ) *PublishResponseContent`

NewPublishResponseContent instantiates a new PublishResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewPublishResponseContentWithDefaults

`func NewPublishResponseContentWithDefaults() *PublishResponseContent`

NewPublishResponseContentWithDefaults instantiates a new PublishResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetFunctionName

`func (o *PublishResponseContent) GetFunctionName() string`

GetFunctionName returns the FunctionName field if non-nil, zero value otherwise.

### GetFunctionNameOk

`func (o *PublishResponseContent) GetFunctionNameOk() (*string, bool)`

GetFunctionNameOk returns a tuple with the FunctionName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetFunctionName

`func (o *PublishResponseContent) SetFunctionName(v string)`

SetFunctionName sets FunctionName field to given value.


### GetPublishedCode

`func (o *PublishResponseContent) GetPublishedCode() string`

GetPublishedCode returns the PublishedCode field if non-nil, zero value otherwise.

### GetPublishedCodeOk

`func (o *PublishResponseContent) GetPublishedCodeOk() (*string, bool)`

GetPublishedCodeOk returns a tuple with the PublishedCode field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPublishedCode

`func (o *PublishResponseContent) SetPublishedCode(v string)`

SetPublishedCode sets PublishedCode field to given value.

### HasPublishedCode

`func (o *PublishResponseContent) HasPublishedCode() bool`

HasPublishedCode returns a boolean if a field has been set.

### GetDraftCode

`func (o *PublishResponseContent) GetDraftCode() string`

GetDraftCode returns the DraftCode field if non-nil, zero value otherwise.

### GetDraftCodeOk

`func (o *PublishResponseContent) GetDraftCodeOk() (*string, bool)`

GetDraftCodeOk returns a tuple with the DraftCode field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDraftCode

`func (o *PublishResponseContent) SetDraftCode(v string)`

SetDraftCode sets DraftCode field to given value.


### GetPublishedRuntimeVersion

`func (o *PublishResponseContent) GetPublishedRuntimeVersion() string`

GetPublishedRuntimeVersion returns the PublishedRuntimeVersion field if non-nil, zero value otherwise.

### GetPublishedRuntimeVersionOk

`func (o *PublishResponseContent) GetPublishedRuntimeVersionOk() (*string, bool)`

GetPublishedRuntimeVersionOk returns a tuple with the PublishedRuntimeVersion field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPublishedRuntimeVersion

`func (o *PublishResponseContent) SetPublishedRuntimeVersion(v string)`

SetPublishedRuntimeVersion sets PublishedRuntimeVersion field to given value.

### HasPublishedRuntimeVersion

`func (o *PublishResponseContent) HasPublishedRuntimeVersion() bool`

HasPublishedRuntimeVersion returns a boolean if a field has been set.

### GetDraftRuntimeVersion

`func (o *PublishResponseContent) GetDraftRuntimeVersion() string`

GetDraftRuntimeVersion returns the DraftRuntimeVersion field if non-nil, zero value otherwise.

### GetDraftRuntimeVersionOk

`func (o *PublishResponseContent) GetDraftRuntimeVersionOk() (*string, bool)`

GetDraftRuntimeVersionOk returns a tuple with the DraftRuntimeVersion field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDraftRuntimeVersion

`func (o *PublishResponseContent) SetDraftRuntimeVersion(v string)`

SetDraftRuntimeVersion sets DraftRuntimeVersion field to given value.


### GetPublishedAt

`func (o *PublishResponseContent) GetPublishedAt() time.Time`

GetPublishedAt returns the PublishedAt field if non-nil, zero value otherwise.

### GetPublishedAtOk

`func (o *PublishResponseContent) GetPublishedAtOk() (*time.Time, bool)`

GetPublishedAtOk returns a tuple with the PublishedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPublishedAt

`func (o *PublishResponseContent) SetPublishedAt(v time.Time)`

SetPublishedAt sets PublishedAt field to given value.

### HasPublishedAt

`func (o *PublishResponseContent) HasPublishedAt() bool`

HasPublishedAt returns a boolean if a field has been set.

### GetDraftEditedAt

`func (o *PublishResponseContent) GetDraftEditedAt() time.Time`

GetDraftEditedAt returns the DraftEditedAt field if non-nil, zero value otherwise.

### GetDraftEditedAtOk

`func (o *PublishResponseContent) GetDraftEditedAtOk() (*time.Time, bool)`

GetDraftEditedAtOk returns a tuple with the DraftEditedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDraftEditedAt

`func (o *PublishResponseContent) SetDraftEditedAt(v time.Time)`

SetDraftEditedAt sets DraftEditedAt field to given value.


### GetPublishedBy

`func (o *PublishResponseContent) GetPublishedBy() string`

GetPublishedBy returns the PublishedBy field if non-nil, zero value otherwise.

### GetPublishedByOk

`func (o *PublishResponseContent) GetPublishedByOk() (*string, bool)`

GetPublishedByOk returns a tuple with the PublishedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPublishedBy

`func (o *PublishResponseContent) SetPublishedBy(v string)`

SetPublishedBy sets PublishedBy field to given value.

### HasPublishedBy

`func (o *PublishResponseContent) HasPublishedBy() bool`

HasPublishedBy returns a boolean if a field has been set.

### GetDraftEditedBy

`func (o *PublishResponseContent) GetDraftEditedBy() string`

GetDraftEditedBy returns the DraftEditedBy field if non-nil, zero value otherwise.

### GetDraftEditedByOk

`func (o *PublishResponseContent) GetDraftEditedByOk() (*string, bool)`

GetDraftEditedByOk returns a tuple with the DraftEditedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDraftEditedBy

`func (o *PublishResponseContent) SetDraftEditedBy(v string)`

SetDraftEditedBy sets DraftEditedBy field to given value.


### GetLastModifiedAt

`func (o *PublishResponseContent) GetLastModifiedAt() time.Time`

GetLastModifiedAt returns the LastModifiedAt field if non-nil, zero value otherwise.

### GetLastModifiedAtOk

`func (o *PublishResponseContent) GetLastModifiedAtOk() (*time.Time, bool)`

GetLastModifiedAtOk returns a tuple with the LastModifiedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedAt

`func (o *PublishResponseContent) SetLastModifiedAt(v time.Time)`

SetLastModifiedAt sets LastModifiedAt field to given value.


### GetLastModifiedBy

`func (o *PublishResponseContent) GetLastModifiedBy() string`

GetLastModifiedBy returns the LastModifiedBy field if non-nil, zero value otherwise.

### GetLastModifiedByOk

`func (o *PublishResponseContent) GetLastModifiedByOk() (*string, bool)`

GetLastModifiedByOk returns a tuple with the LastModifiedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedBy

`func (o *PublishResponseContent) SetLastModifiedBy(v string)`

SetLastModifiedBy sets LastModifiedBy field to given value.


### GetChangeReason

`func (o *PublishResponseContent) GetChangeReason() string`

GetChangeReason returns the ChangeReason field if non-nil, zero value otherwise.

### GetChangeReasonOk

`func (o *PublishResponseContent) GetChangeReasonOk() (*string, bool)`

GetChangeReasonOk returns a tuple with the ChangeReason field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetChangeReason

`func (o *PublishResponseContent) SetChangeReason(v string)`

SetChangeReason sets ChangeReason field to given value.


### GetDescription

`func (o *PublishResponseContent) GetDescription() string`

GetDescription returns the Description field if non-nil, zero value otherwise.

### GetDescriptionOk

`func (o *PublishResponseContent) GetDescriptionOk() (*string, bool)`

GetDescriptionOk returns a tuple with the Description field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDescription

`func (o *PublishResponseContent) SetDescription(v string)`

SetDescription sets Description field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


