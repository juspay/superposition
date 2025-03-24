# UpdateWorkspaceResponseContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**WorkspaceName** | **string** |  | 
**OrganisationId** | **string** |  | 
**OrganisationName** | **string** |  | 
**WorkspaceSchemaName** | **string** |  | 
**WorkspaceStatus** | **string** |  | 
**WorkspaceAdminEmail** | **string** |  | 
**CreatedBy** | **string** |  | 
**LastModifiedBy** | **string** |  | 
**LastModifiedAt** | **time.Time** |  | 
**CreatedAt** | **time.Time** |  | 
**MandatoryDimensions** | Pointer to **[]string** |  | [optional] 

## Methods

### NewUpdateWorkspaceResponseContent

`func NewUpdateWorkspaceResponseContent(workspaceName string, organisationId string, organisationName string, workspaceSchemaName string, workspaceStatus string, workspaceAdminEmail string, createdBy string, lastModifiedBy string, lastModifiedAt time.Time, createdAt time.Time, ) *UpdateWorkspaceResponseContent`

NewUpdateWorkspaceResponseContent instantiates a new UpdateWorkspaceResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewUpdateWorkspaceResponseContentWithDefaults

`func NewUpdateWorkspaceResponseContentWithDefaults() *UpdateWorkspaceResponseContent`

NewUpdateWorkspaceResponseContentWithDefaults instantiates a new UpdateWorkspaceResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetWorkspaceName

`func (o *UpdateWorkspaceResponseContent) GetWorkspaceName() string`

GetWorkspaceName returns the WorkspaceName field if non-nil, zero value otherwise.

### GetWorkspaceNameOk

`func (o *UpdateWorkspaceResponseContent) GetWorkspaceNameOk() (*string, bool)`

GetWorkspaceNameOk returns a tuple with the WorkspaceName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetWorkspaceName

`func (o *UpdateWorkspaceResponseContent) SetWorkspaceName(v string)`

SetWorkspaceName sets WorkspaceName field to given value.


### GetOrganisationId

`func (o *UpdateWorkspaceResponseContent) GetOrganisationId() string`

GetOrganisationId returns the OrganisationId field if non-nil, zero value otherwise.

### GetOrganisationIdOk

`func (o *UpdateWorkspaceResponseContent) GetOrganisationIdOk() (*string, bool)`

GetOrganisationIdOk returns a tuple with the OrganisationId field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOrganisationId

`func (o *UpdateWorkspaceResponseContent) SetOrganisationId(v string)`

SetOrganisationId sets OrganisationId field to given value.


### GetOrganisationName

`func (o *UpdateWorkspaceResponseContent) GetOrganisationName() string`

GetOrganisationName returns the OrganisationName field if non-nil, zero value otherwise.

### GetOrganisationNameOk

`func (o *UpdateWorkspaceResponseContent) GetOrganisationNameOk() (*string, bool)`

GetOrganisationNameOk returns a tuple with the OrganisationName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOrganisationName

`func (o *UpdateWorkspaceResponseContent) SetOrganisationName(v string)`

SetOrganisationName sets OrganisationName field to given value.


### GetWorkspaceSchemaName

`func (o *UpdateWorkspaceResponseContent) GetWorkspaceSchemaName() string`

GetWorkspaceSchemaName returns the WorkspaceSchemaName field if non-nil, zero value otherwise.

### GetWorkspaceSchemaNameOk

`func (o *UpdateWorkspaceResponseContent) GetWorkspaceSchemaNameOk() (*string, bool)`

GetWorkspaceSchemaNameOk returns a tuple with the WorkspaceSchemaName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetWorkspaceSchemaName

`func (o *UpdateWorkspaceResponseContent) SetWorkspaceSchemaName(v string)`

SetWorkspaceSchemaName sets WorkspaceSchemaName field to given value.


### GetWorkspaceStatus

`func (o *UpdateWorkspaceResponseContent) GetWorkspaceStatus() string`

GetWorkspaceStatus returns the WorkspaceStatus field if non-nil, zero value otherwise.

### GetWorkspaceStatusOk

`func (o *UpdateWorkspaceResponseContent) GetWorkspaceStatusOk() (*string, bool)`

GetWorkspaceStatusOk returns a tuple with the WorkspaceStatus field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetWorkspaceStatus

`func (o *UpdateWorkspaceResponseContent) SetWorkspaceStatus(v string)`

SetWorkspaceStatus sets WorkspaceStatus field to given value.


### GetWorkspaceAdminEmail

`func (o *UpdateWorkspaceResponseContent) GetWorkspaceAdminEmail() string`

GetWorkspaceAdminEmail returns the WorkspaceAdminEmail field if non-nil, zero value otherwise.

### GetWorkspaceAdminEmailOk

`func (o *UpdateWorkspaceResponseContent) GetWorkspaceAdminEmailOk() (*string, bool)`

GetWorkspaceAdminEmailOk returns a tuple with the WorkspaceAdminEmail field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetWorkspaceAdminEmail

`func (o *UpdateWorkspaceResponseContent) SetWorkspaceAdminEmail(v string)`

SetWorkspaceAdminEmail sets WorkspaceAdminEmail field to given value.


### GetCreatedBy

`func (o *UpdateWorkspaceResponseContent) GetCreatedBy() string`

GetCreatedBy returns the CreatedBy field if non-nil, zero value otherwise.

### GetCreatedByOk

`func (o *UpdateWorkspaceResponseContent) GetCreatedByOk() (*string, bool)`

GetCreatedByOk returns a tuple with the CreatedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedBy

`func (o *UpdateWorkspaceResponseContent) SetCreatedBy(v string)`

SetCreatedBy sets CreatedBy field to given value.


### GetLastModifiedBy

`func (o *UpdateWorkspaceResponseContent) GetLastModifiedBy() string`

GetLastModifiedBy returns the LastModifiedBy field if non-nil, zero value otherwise.

### GetLastModifiedByOk

`func (o *UpdateWorkspaceResponseContent) GetLastModifiedByOk() (*string, bool)`

GetLastModifiedByOk returns a tuple with the LastModifiedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedBy

`func (o *UpdateWorkspaceResponseContent) SetLastModifiedBy(v string)`

SetLastModifiedBy sets LastModifiedBy field to given value.


### GetLastModifiedAt

`func (o *UpdateWorkspaceResponseContent) GetLastModifiedAt() time.Time`

GetLastModifiedAt returns the LastModifiedAt field if non-nil, zero value otherwise.

### GetLastModifiedAtOk

`func (o *UpdateWorkspaceResponseContent) GetLastModifiedAtOk() (*time.Time, bool)`

GetLastModifiedAtOk returns a tuple with the LastModifiedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedAt

`func (o *UpdateWorkspaceResponseContent) SetLastModifiedAt(v time.Time)`

SetLastModifiedAt sets LastModifiedAt field to given value.


### GetCreatedAt

`func (o *UpdateWorkspaceResponseContent) GetCreatedAt() time.Time`

GetCreatedAt returns the CreatedAt field if non-nil, zero value otherwise.

### GetCreatedAtOk

`func (o *UpdateWorkspaceResponseContent) GetCreatedAtOk() (*time.Time, bool)`

GetCreatedAtOk returns a tuple with the CreatedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedAt

`func (o *UpdateWorkspaceResponseContent) SetCreatedAt(v time.Time)`

SetCreatedAt sets CreatedAt field to given value.


### GetMandatoryDimensions

`func (o *UpdateWorkspaceResponseContent) GetMandatoryDimensions() []string`

GetMandatoryDimensions returns the MandatoryDimensions field if non-nil, zero value otherwise.

### GetMandatoryDimensionsOk

`func (o *UpdateWorkspaceResponseContent) GetMandatoryDimensionsOk() (*[]string, bool)`

GetMandatoryDimensionsOk returns a tuple with the MandatoryDimensions field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetMandatoryDimensions

`func (o *UpdateWorkspaceResponseContent) SetMandatoryDimensions(v []string)`

SetMandatoryDimensions sets MandatoryDimensions field to given value.

### HasMandatoryDimensions

`func (o *UpdateWorkspaceResponseContent) HasMandatoryDimensions() bool`

HasMandatoryDimensions returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


