# CreateWorkspaceResponseContent

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

### NewCreateWorkspaceResponseContent

`func NewCreateWorkspaceResponseContent(workspaceName string, organisationId string, organisationName string, workspaceSchemaName string, workspaceStatus string, workspaceAdminEmail string, createdBy string, lastModifiedBy string, lastModifiedAt time.Time, createdAt time.Time, ) *CreateWorkspaceResponseContent`

NewCreateWorkspaceResponseContent instantiates a new CreateWorkspaceResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewCreateWorkspaceResponseContentWithDefaults

`func NewCreateWorkspaceResponseContentWithDefaults() *CreateWorkspaceResponseContent`

NewCreateWorkspaceResponseContentWithDefaults instantiates a new CreateWorkspaceResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetWorkspaceName

`func (o *CreateWorkspaceResponseContent) GetWorkspaceName() string`

GetWorkspaceName returns the WorkspaceName field if non-nil, zero value otherwise.

### GetWorkspaceNameOk

`func (o *CreateWorkspaceResponseContent) GetWorkspaceNameOk() (*string, bool)`

GetWorkspaceNameOk returns a tuple with the WorkspaceName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetWorkspaceName

`func (o *CreateWorkspaceResponseContent) SetWorkspaceName(v string)`

SetWorkspaceName sets WorkspaceName field to given value.


### GetOrganisationId

`func (o *CreateWorkspaceResponseContent) GetOrganisationId() string`

GetOrganisationId returns the OrganisationId field if non-nil, zero value otherwise.

### GetOrganisationIdOk

`func (o *CreateWorkspaceResponseContent) GetOrganisationIdOk() (*string, bool)`

GetOrganisationIdOk returns a tuple with the OrganisationId field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOrganisationId

`func (o *CreateWorkspaceResponseContent) SetOrganisationId(v string)`

SetOrganisationId sets OrganisationId field to given value.


### GetOrganisationName

`func (o *CreateWorkspaceResponseContent) GetOrganisationName() string`

GetOrganisationName returns the OrganisationName field if non-nil, zero value otherwise.

### GetOrganisationNameOk

`func (o *CreateWorkspaceResponseContent) GetOrganisationNameOk() (*string, bool)`

GetOrganisationNameOk returns a tuple with the OrganisationName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOrganisationName

`func (o *CreateWorkspaceResponseContent) SetOrganisationName(v string)`

SetOrganisationName sets OrganisationName field to given value.


### GetWorkspaceSchemaName

`func (o *CreateWorkspaceResponseContent) GetWorkspaceSchemaName() string`

GetWorkspaceSchemaName returns the WorkspaceSchemaName field if non-nil, zero value otherwise.

### GetWorkspaceSchemaNameOk

`func (o *CreateWorkspaceResponseContent) GetWorkspaceSchemaNameOk() (*string, bool)`

GetWorkspaceSchemaNameOk returns a tuple with the WorkspaceSchemaName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetWorkspaceSchemaName

`func (o *CreateWorkspaceResponseContent) SetWorkspaceSchemaName(v string)`

SetWorkspaceSchemaName sets WorkspaceSchemaName field to given value.


### GetWorkspaceStatus

`func (o *CreateWorkspaceResponseContent) GetWorkspaceStatus() string`

GetWorkspaceStatus returns the WorkspaceStatus field if non-nil, zero value otherwise.

### GetWorkspaceStatusOk

`func (o *CreateWorkspaceResponseContent) GetWorkspaceStatusOk() (*string, bool)`

GetWorkspaceStatusOk returns a tuple with the WorkspaceStatus field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetWorkspaceStatus

`func (o *CreateWorkspaceResponseContent) SetWorkspaceStatus(v string)`

SetWorkspaceStatus sets WorkspaceStatus field to given value.


### GetWorkspaceAdminEmail

`func (o *CreateWorkspaceResponseContent) GetWorkspaceAdminEmail() string`

GetWorkspaceAdminEmail returns the WorkspaceAdminEmail field if non-nil, zero value otherwise.

### GetWorkspaceAdminEmailOk

`func (o *CreateWorkspaceResponseContent) GetWorkspaceAdminEmailOk() (*string, bool)`

GetWorkspaceAdminEmailOk returns a tuple with the WorkspaceAdminEmail field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetWorkspaceAdminEmail

`func (o *CreateWorkspaceResponseContent) SetWorkspaceAdminEmail(v string)`

SetWorkspaceAdminEmail sets WorkspaceAdminEmail field to given value.


### GetCreatedBy

`func (o *CreateWorkspaceResponseContent) GetCreatedBy() string`

GetCreatedBy returns the CreatedBy field if non-nil, zero value otherwise.

### GetCreatedByOk

`func (o *CreateWorkspaceResponseContent) GetCreatedByOk() (*string, bool)`

GetCreatedByOk returns a tuple with the CreatedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedBy

`func (o *CreateWorkspaceResponseContent) SetCreatedBy(v string)`

SetCreatedBy sets CreatedBy field to given value.


### GetLastModifiedBy

`func (o *CreateWorkspaceResponseContent) GetLastModifiedBy() string`

GetLastModifiedBy returns the LastModifiedBy field if non-nil, zero value otherwise.

### GetLastModifiedByOk

`func (o *CreateWorkspaceResponseContent) GetLastModifiedByOk() (*string, bool)`

GetLastModifiedByOk returns a tuple with the LastModifiedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedBy

`func (o *CreateWorkspaceResponseContent) SetLastModifiedBy(v string)`

SetLastModifiedBy sets LastModifiedBy field to given value.


### GetLastModifiedAt

`func (o *CreateWorkspaceResponseContent) GetLastModifiedAt() time.Time`

GetLastModifiedAt returns the LastModifiedAt field if non-nil, zero value otherwise.

### GetLastModifiedAtOk

`func (o *CreateWorkspaceResponseContent) GetLastModifiedAtOk() (*time.Time, bool)`

GetLastModifiedAtOk returns a tuple with the LastModifiedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedAt

`func (o *CreateWorkspaceResponseContent) SetLastModifiedAt(v time.Time)`

SetLastModifiedAt sets LastModifiedAt field to given value.


### GetCreatedAt

`func (o *CreateWorkspaceResponseContent) GetCreatedAt() time.Time`

GetCreatedAt returns the CreatedAt field if non-nil, zero value otherwise.

### GetCreatedAtOk

`func (o *CreateWorkspaceResponseContent) GetCreatedAtOk() (*time.Time, bool)`

GetCreatedAtOk returns a tuple with the CreatedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedAt

`func (o *CreateWorkspaceResponseContent) SetCreatedAt(v time.Time)`

SetCreatedAt sets CreatedAt field to given value.


### GetMandatoryDimensions

`func (o *CreateWorkspaceResponseContent) GetMandatoryDimensions() []string`

GetMandatoryDimensions returns the MandatoryDimensions field if non-nil, zero value otherwise.

### GetMandatoryDimensionsOk

`func (o *CreateWorkspaceResponseContent) GetMandatoryDimensionsOk() (*[]string, bool)`

GetMandatoryDimensionsOk returns a tuple with the MandatoryDimensions field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetMandatoryDimensions

`func (o *CreateWorkspaceResponseContent) SetMandatoryDimensions(v []string)`

SetMandatoryDimensions sets MandatoryDimensions field to given value.

### HasMandatoryDimensions

`func (o *CreateWorkspaceResponseContent) HasMandatoryDimensions() bool`

HasMandatoryDimensions returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


