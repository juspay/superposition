# WorkspaceResponse

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

### NewWorkspaceResponse

`func NewWorkspaceResponse(workspaceName string, organisationId string, organisationName string, workspaceSchemaName string, workspaceStatus string, workspaceAdminEmail string, createdBy string, lastModifiedBy string, lastModifiedAt time.Time, createdAt time.Time, ) *WorkspaceResponse`

NewWorkspaceResponse instantiates a new WorkspaceResponse object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewWorkspaceResponseWithDefaults

`func NewWorkspaceResponseWithDefaults() *WorkspaceResponse`

NewWorkspaceResponseWithDefaults instantiates a new WorkspaceResponse object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetWorkspaceName

`func (o *WorkspaceResponse) GetWorkspaceName() string`

GetWorkspaceName returns the WorkspaceName field if non-nil, zero value otherwise.

### GetWorkspaceNameOk

`func (o *WorkspaceResponse) GetWorkspaceNameOk() (*string, bool)`

GetWorkspaceNameOk returns a tuple with the WorkspaceName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetWorkspaceName

`func (o *WorkspaceResponse) SetWorkspaceName(v string)`

SetWorkspaceName sets WorkspaceName field to given value.


### GetOrganisationId

`func (o *WorkspaceResponse) GetOrganisationId() string`

GetOrganisationId returns the OrganisationId field if non-nil, zero value otherwise.

### GetOrganisationIdOk

`func (o *WorkspaceResponse) GetOrganisationIdOk() (*string, bool)`

GetOrganisationIdOk returns a tuple with the OrganisationId field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOrganisationId

`func (o *WorkspaceResponse) SetOrganisationId(v string)`

SetOrganisationId sets OrganisationId field to given value.


### GetOrganisationName

`func (o *WorkspaceResponse) GetOrganisationName() string`

GetOrganisationName returns the OrganisationName field if non-nil, zero value otherwise.

### GetOrganisationNameOk

`func (o *WorkspaceResponse) GetOrganisationNameOk() (*string, bool)`

GetOrganisationNameOk returns a tuple with the OrganisationName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOrganisationName

`func (o *WorkspaceResponse) SetOrganisationName(v string)`

SetOrganisationName sets OrganisationName field to given value.


### GetWorkspaceSchemaName

`func (o *WorkspaceResponse) GetWorkspaceSchemaName() string`

GetWorkspaceSchemaName returns the WorkspaceSchemaName field if non-nil, zero value otherwise.

### GetWorkspaceSchemaNameOk

`func (o *WorkspaceResponse) GetWorkspaceSchemaNameOk() (*string, bool)`

GetWorkspaceSchemaNameOk returns a tuple with the WorkspaceSchemaName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetWorkspaceSchemaName

`func (o *WorkspaceResponse) SetWorkspaceSchemaName(v string)`

SetWorkspaceSchemaName sets WorkspaceSchemaName field to given value.


### GetWorkspaceStatus

`func (o *WorkspaceResponse) GetWorkspaceStatus() string`

GetWorkspaceStatus returns the WorkspaceStatus field if non-nil, zero value otherwise.

### GetWorkspaceStatusOk

`func (o *WorkspaceResponse) GetWorkspaceStatusOk() (*string, bool)`

GetWorkspaceStatusOk returns a tuple with the WorkspaceStatus field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetWorkspaceStatus

`func (o *WorkspaceResponse) SetWorkspaceStatus(v string)`

SetWorkspaceStatus sets WorkspaceStatus field to given value.


### GetWorkspaceAdminEmail

`func (o *WorkspaceResponse) GetWorkspaceAdminEmail() string`

GetWorkspaceAdminEmail returns the WorkspaceAdminEmail field if non-nil, zero value otherwise.

### GetWorkspaceAdminEmailOk

`func (o *WorkspaceResponse) GetWorkspaceAdminEmailOk() (*string, bool)`

GetWorkspaceAdminEmailOk returns a tuple with the WorkspaceAdminEmail field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetWorkspaceAdminEmail

`func (o *WorkspaceResponse) SetWorkspaceAdminEmail(v string)`

SetWorkspaceAdminEmail sets WorkspaceAdminEmail field to given value.


### GetCreatedBy

`func (o *WorkspaceResponse) GetCreatedBy() string`

GetCreatedBy returns the CreatedBy field if non-nil, zero value otherwise.

### GetCreatedByOk

`func (o *WorkspaceResponse) GetCreatedByOk() (*string, bool)`

GetCreatedByOk returns a tuple with the CreatedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedBy

`func (o *WorkspaceResponse) SetCreatedBy(v string)`

SetCreatedBy sets CreatedBy field to given value.


### GetLastModifiedBy

`func (o *WorkspaceResponse) GetLastModifiedBy() string`

GetLastModifiedBy returns the LastModifiedBy field if non-nil, zero value otherwise.

### GetLastModifiedByOk

`func (o *WorkspaceResponse) GetLastModifiedByOk() (*string, bool)`

GetLastModifiedByOk returns a tuple with the LastModifiedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedBy

`func (o *WorkspaceResponse) SetLastModifiedBy(v string)`

SetLastModifiedBy sets LastModifiedBy field to given value.


### GetLastModifiedAt

`func (o *WorkspaceResponse) GetLastModifiedAt() time.Time`

GetLastModifiedAt returns the LastModifiedAt field if non-nil, zero value otherwise.

### GetLastModifiedAtOk

`func (o *WorkspaceResponse) GetLastModifiedAtOk() (*time.Time, bool)`

GetLastModifiedAtOk returns a tuple with the LastModifiedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetLastModifiedAt

`func (o *WorkspaceResponse) SetLastModifiedAt(v time.Time)`

SetLastModifiedAt sets LastModifiedAt field to given value.


### GetCreatedAt

`func (o *WorkspaceResponse) GetCreatedAt() time.Time`

GetCreatedAt returns the CreatedAt field if non-nil, zero value otherwise.

### GetCreatedAtOk

`func (o *WorkspaceResponse) GetCreatedAtOk() (*time.Time, bool)`

GetCreatedAtOk returns a tuple with the CreatedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedAt

`func (o *WorkspaceResponse) SetCreatedAt(v time.Time)`

SetCreatedAt sets CreatedAt field to given value.


### GetMandatoryDimensions

`func (o *WorkspaceResponse) GetMandatoryDimensions() []string`

GetMandatoryDimensions returns the MandatoryDimensions field if non-nil, zero value otherwise.

### GetMandatoryDimensionsOk

`func (o *WorkspaceResponse) GetMandatoryDimensionsOk() (*[]string, bool)`

GetMandatoryDimensionsOk returns a tuple with the MandatoryDimensions field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetMandatoryDimensions

`func (o *WorkspaceResponse) SetMandatoryDimensions(v []string)`

SetMandatoryDimensions sets MandatoryDimensions field to given value.

### HasMandatoryDimensions

`func (o *WorkspaceResponse) HasMandatoryDimensions() bool`

HasMandatoryDimensions returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


