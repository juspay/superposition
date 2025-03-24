# CreaterOrganisationResponseContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Id** | **string** |  | 
**Name** | **string** |  | 
**CountryCode** | Pointer to **string** |  | [optional] 
**ContactEmail** | Pointer to **string** |  | [optional] 
**ContactPhone** | Pointer to **string** |  | [optional] 
**CreatedBy** | **string** |  | 
**AdminEmail** | **string** |  | 
**Status** | **string** |  | 
**Sector** | Pointer to **string** |  | [optional] 
**CreatedAt** | **time.Time** |  | 
**UpdatedAt** | **time.Time** |  | 
**UpdatedBy** | **string** |  | 

## Methods

### NewCreaterOrganisationResponseContent

`func NewCreaterOrganisationResponseContent(id string, name string, createdBy string, adminEmail string, status string, createdAt time.Time, updatedAt time.Time, updatedBy string, ) *CreaterOrganisationResponseContent`

NewCreaterOrganisationResponseContent instantiates a new CreaterOrganisationResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewCreaterOrganisationResponseContentWithDefaults

`func NewCreaterOrganisationResponseContentWithDefaults() *CreaterOrganisationResponseContent`

NewCreaterOrganisationResponseContentWithDefaults instantiates a new CreaterOrganisationResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetId

`func (o *CreaterOrganisationResponseContent) GetId() string`

GetId returns the Id field if non-nil, zero value otherwise.

### GetIdOk

`func (o *CreaterOrganisationResponseContent) GetIdOk() (*string, bool)`

GetIdOk returns a tuple with the Id field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetId

`func (o *CreaterOrganisationResponseContent) SetId(v string)`

SetId sets Id field to given value.


### GetName

`func (o *CreaterOrganisationResponseContent) GetName() string`

GetName returns the Name field if non-nil, zero value otherwise.

### GetNameOk

`func (o *CreaterOrganisationResponseContent) GetNameOk() (*string, bool)`

GetNameOk returns a tuple with the Name field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetName

`func (o *CreaterOrganisationResponseContent) SetName(v string)`

SetName sets Name field to given value.


### GetCountryCode

`func (o *CreaterOrganisationResponseContent) GetCountryCode() string`

GetCountryCode returns the CountryCode field if non-nil, zero value otherwise.

### GetCountryCodeOk

`func (o *CreaterOrganisationResponseContent) GetCountryCodeOk() (*string, bool)`

GetCountryCodeOk returns a tuple with the CountryCode field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCountryCode

`func (o *CreaterOrganisationResponseContent) SetCountryCode(v string)`

SetCountryCode sets CountryCode field to given value.

### HasCountryCode

`func (o *CreaterOrganisationResponseContent) HasCountryCode() bool`

HasCountryCode returns a boolean if a field has been set.

### GetContactEmail

`func (o *CreaterOrganisationResponseContent) GetContactEmail() string`

GetContactEmail returns the ContactEmail field if non-nil, zero value otherwise.

### GetContactEmailOk

`func (o *CreaterOrganisationResponseContent) GetContactEmailOk() (*string, bool)`

GetContactEmailOk returns a tuple with the ContactEmail field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetContactEmail

`func (o *CreaterOrganisationResponseContent) SetContactEmail(v string)`

SetContactEmail sets ContactEmail field to given value.

### HasContactEmail

`func (o *CreaterOrganisationResponseContent) HasContactEmail() bool`

HasContactEmail returns a boolean if a field has been set.

### GetContactPhone

`func (o *CreaterOrganisationResponseContent) GetContactPhone() string`

GetContactPhone returns the ContactPhone field if non-nil, zero value otherwise.

### GetContactPhoneOk

`func (o *CreaterOrganisationResponseContent) GetContactPhoneOk() (*string, bool)`

GetContactPhoneOk returns a tuple with the ContactPhone field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetContactPhone

`func (o *CreaterOrganisationResponseContent) SetContactPhone(v string)`

SetContactPhone sets ContactPhone field to given value.

### HasContactPhone

`func (o *CreaterOrganisationResponseContent) HasContactPhone() bool`

HasContactPhone returns a boolean if a field has been set.

### GetCreatedBy

`func (o *CreaterOrganisationResponseContent) GetCreatedBy() string`

GetCreatedBy returns the CreatedBy field if non-nil, zero value otherwise.

### GetCreatedByOk

`func (o *CreaterOrganisationResponseContent) GetCreatedByOk() (*string, bool)`

GetCreatedByOk returns a tuple with the CreatedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedBy

`func (o *CreaterOrganisationResponseContent) SetCreatedBy(v string)`

SetCreatedBy sets CreatedBy field to given value.


### GetAdminEmail

`func (o *CreaterOrganisationResponseContent) GetAdminEmail() string`

GetAdminEmail returns the AdminEmail field if non-nil, zero value otherwise.

### GetAdminEmailOk

`func (o *CreaterOrganisationResponseContent) GetAdminEmailOk() (*string, bool)`

GetAdminEmailOk returns a tuple with the AdminEmail field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetAdminEmail

`func (o *CreaterOrganisationResponseContent) SetAdminEmail(v string)`

SetAdminEmail sets AdminEmail field to given value.


### GetStatus

`func (o *CreaterOrganisationResponseContent) GetStatus() string`

GetStatus returns the Status field if non-nil, zero value otherwise.

### GetStatusOk

`func (o *CreaterOrganisationResponseContent) GetStatusOk() (*string, bool)`

GetStatusOk returns a tuple with the Status field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetStatus

`func (o *CreaterOrganisationResponseContent) SetStatus(v string)`

SetStatus sets Status field to given value.


### GetSector

`func (o *CreaterOrganisationResponseContent) GetSector() string`

GetSector returns the Sector field if non-nil, zero value otherwise.

### GetSectorOk

`func (o *CreaterOrganisationResponseContent) GetSectorOk() (*string, bool)`

GetSectorOk returns a tuple with the Sector field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetSector

`func (o *CreaterOrganisationResponseContent) SetSector(v string)`

SetSector sets Sector field to given value.

### HasSector

`func (o *CreaterOrganisationResponseContent) HasSector() bool`

HasSector returns a boolean if a field has been set.

### GetCreatedAt

`func (o *CreaterOrganisationResponseContent) GetCreatedAt() time.Time`

GetCreatedAt returns the CreatedAt field if non-nil, zero value otherwise.

### GetCreatedAtOk

`func (o *CreaterOrganisationResponseContent) GetCreatedAtOk() (*time.Time, bool)`

GetCreatedAtOk returns a tuple with the CreatedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedAt

`func (o *CreaterOrganisationResponseContent) SetCreatedAt(v time.Time)`

SetCreatedAt sets CreatedAt field to given value.


### GetUpdatedAt

`func (o *CreaterOrganisationResponseContent) GetUpdatedAt() time.Time`

GetUpdatedAt returns the UpdatedAt field if non-nil, zero value otherwise.

### GetUpdatedAtOk

`func (o *CreaterOrganisationResponseContent) GetUpdatedAtOk() (*time.Time, bool)`

GetUpdatedAtOk returns a tuple with the UpdatedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetUpdatedAt

`func (o *CreaterOrganisationResponseContent) SetUpdatedAt(v time.Time)`

SetUpdatedAt sets UpdatedAt field to given value.


### GetUpdatedBy

`func (o *CreaterOrganisationResponseContent) GetUpdatedBy() string`

GetUpdatedBy returns the UpdatedBy field if non-nil, zero value otherwise.

### GetUpdatedByOk

`func (o *CreaterOrganisationResponseContent) GetUpdatedByOk() (*string, bool)`

GetUpdatedByOk returns a tuple with the UpdatedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetUpdatedBy

`func (o *CreaterOrganisationResponseContent) SetUpdatedBy(v string)`

SetUpdatedBy sets UpdatedBy field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


