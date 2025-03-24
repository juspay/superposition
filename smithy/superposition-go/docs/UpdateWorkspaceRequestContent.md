# UpdateWorkspaceRequestContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**WorkspaceAdminEmail** | **string** |  | 
**MandatoryDimensions** | Pointer to **[]string** |  | [optional] 
**WorkspaceStatus** | Pointer to **string** |  | [optional] 

## Methods

### NewUpdateWorkspaceRequestContent

`func NewUpdateWorkspaceRequestContent(workspaceAdminEmail string, ) *UpdateWorkspaceRequestContent`

NewUpdateWorkspaceRequestContent instantiates a new UpdateWorkspaceRequestContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewUpdateWorkspaceRequestContentWithDefaults

`func NewUpdateWorkspaceRequestContentWithDefaults() *UpdateWorkspaceRequestContent`

NewUpdateWorkspaceRequestContentWithDefaults instantiates a new UpdateWorkspaceRequestContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetWorkspaceAdminEmail

`func (o *UpdateWorkspaceRequestContent) GetWorkspaceAdminEmail() string`

GetWorkspaceAdminEmail returns the WorkspaceAdminEmail field if non-nil, zero value otherwise.

### GetWorkspaceAdminEmailOk

`func (o *UpdateWorkspaceRequestContent) GetWorkspaceAdminEmailOk() (*string, bool)`

GetWorkspaceAdminEmailOk returns a tuple with the WorkspaceAdminEmail field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetWorkspaceAdminEmail

`func (o *UpdateWorkspaceRequestContent) SetWorkspaceAdminEmail(v string)`

SetWorkspaceAdminEmail sets WorkspaceAdminEmail field to given value.


### GetMandatoryDimensions

`func (o *UpdateWorkspaceRequestContent) GetMandatoryDimensions() []string`

GetMandatoryDimensions returns the MandatoryDimensions field if non-nil, zero value otherwise.

### GetMandatoryDimensionsOk

`func (o *UpdateWorkspaceRequestContent) GetMandatoryDimensionsOk() (*[]string, bool)`

GetMandatoryDimensionsOk returns a tuple with the MandatoryDimensions field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetMandatoryDimensions

`func (o *UpdateWorkspaceRequestContent) SetMandatoryDimensions(v []string)`

SetMandatoryDimensions sets MandatoryDimensions field to given value.

### HasMandatoryDimensions

`func (o *UpdateWorkspaceRequestContent) HasMandatoryDimensions() bool`

HasMandatoryDimensions returns a boolean if a field has been set.

### GetWorkspaceStatus

`func (o *UpdateWorkspaceRequestContent) GetWorkspaceStatus() string`

GetWorkspaceStatus returns the WorkspaceStatus field if non-nil, zero value otherwise.

### GetWorkspaceStatusOk

`func (o *UpdateWorkspaceRequestContent) GetWorkspaceStatusOk() (*string, bool)`

GetWorkspaceStatusOk returns a tuple with the WorkspaceStatus field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetWorkspaceStatus

`func (o *UpdateWorkspaceRequestContent) SetWorkspaceStatus(v string)`

SetWorkspaceStatus sets WorkspaceStatus field to given value.

### HasWorkspaceStatus

`func (o *UpdateWorkspaceRequestContent) HasWorkspaceStatus() bool`

HasWorkspaceStatus returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


