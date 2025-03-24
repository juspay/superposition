# UpdateOrganisationResponseContent

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

### NewUpdateOrganisationResponseContent

`func NewUpdateOrganisationResponseContent(id string, name string, createdBy string, adminEmail string, status string, createdAt time.Time, updatedAt time.Time, updatedBy string, ) *UpdateOrganisationResponseContent`

NewUpdateOrganisationResponseContent instantiates a new UpdateOrganisationResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewUpdateOrganisationResponseContentWithDefaults

`func NewUpdateOrganisationResponseContentWithDefaults() *UpdateOrganisationResponseContent`

NewUpdateOrganisationResponseContentWithDefaults instantiates a new UpdateOrganisationResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetId

`func (o *UpdateOrganisationResponseContent) GetId() string`

GetId returns the Id field if non-nil, zero value otherwise.

### GetIdOk

`func (o *UpdateOrganisationResponseContent) GetIdOk() (*string, bool)`

GetIdOk returns a tuple with the Id field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetId

`func (o *UpdateOrganisationResponseContent) SetId(v string)`

SetId sets Id field to given value.


### GetName

`func (o *UpdateOrganisationResponseContent) GetName() string`

GetName returns the Name field if non-nil, zero value otherwise.

### GetNameOk

`func (o *UpdateOrganisationResponseContent) GetNameOk() (*string, bool)`

GetNameOk returns a tuple with the Name field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetName

`func (o *UpdateOrganisationResponseContent) SetName(v string)`

SetName sets Name field to given value.


### GetCountryCode

`func (o *UpdateOrganisationResponseContent) GetCountryCode() string`

GetCountryCode returns the CountryCode field if non-nil, zero value otherwise.

### GetCountryCodeOk

`func (o *UpdateOrganisationResponseContent) GetCountryCodeOk() (*string, bool)`

GetCountryCodeOk returns a tuple with the CountryCode field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCountryCode

`func (o *UpdateOrganisationResponseContent) SetCountryCode(v string)`

SetCountryCode sets CountryCode field to given value.

### HasCountryCode

`func (o *UpdateOrganisationResponseContent) HasCountryCode() bool`

HasCountryCode returns a boolean if a field has been set.

### GetContactEmail

`func (o *UpdateOrganisationResponseContent) GetContactEmail() string`

GetContactEmail returns the ContactEmail field if non-nil, zero value otherwise.

### GetContactEmailOk

`func (o *UpdateOrganisationResponseContent) GetContactEmailOk() (*string, bool)`

GetContactEmailOk returns a tuple with the ContactEmail field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetContactEmail

`func (o *UpdateOrganisationResponseContent) SetContactEmail(v string)`

SetContactEmail sets ContactEmail field to given value.

### HasContactEmail

`func (o *UpdateOrganisationResponseContent) HasContactEmail() bool`

HasContactEmail returns a boolean if a field has been set.

### GetContactPhone

`func (o *UpdateOrganisationResponseContent) GetContactPhone() string`

GetContactPhone returns the ContactPhone field if non-nil, zero value otherwise.

### GetContactPhoneOk

`func (o *UpdateOrganisationResponseContent) GetContactPhoneOk() (*string, bool)`

GetContactPhoneOk returns a tuple with the ContactPhone field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetContactPhone

`func (o *UpdateOrganisationResponseContent) SetContactPhone(v string)`

SetContactPhone sets ContactPhone field to given value.

### HasContactPhone

`func (o *UpdateOrganisationResponseContent) HasContactPhone() bool`

HasContactPhone returns a boolean if a field has been set.

### GetCreatedBy

`func (o *UpdateOrganisationResponseContent) GetCreatedBy() string`

GetCreatedBy returns the CreatedBy field if non-nil, zero value otherwise.

### GetCreatedByOk

`func (o *UpdateOrganisationResponseContent) GetCreatedByOk() (*string, bool)`

GetCreatedByOk returns a tuple with the CreatedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedBy

`func (o *UpdateOrganisationResponseContent) SetCreatedBy(v string)`

SetCreatedBy sets CreatedBy field to given value.


### GetAdminEmail

`func (o *UpdateOrganisationResponseContent) GetAdminEmail() string`

GetAdminEmail returns the AdminEmail field if non-nil, zero value otherwise.

### GetAdminEmailOk

`func (o *UpdateOrganisationResponseContent) GetAdminEmailOk() (*string, bool)`

GetAdminEmailOk returns a tuple with the AdminEmail field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetAdminEmail

`func (o *UpdateOrganisationResponseContent) SetAdminEmail(v string)`

SetAdminEmail sets AdminEmail field to given value.


### GetStatus

`func (o *UpdateOrganisationResponseContent) GetStatus() string`

GetStatus returns the Status field if non-nil, zero value otherwise.

### GetStatusOk

`func (o *UpdateOrganisationResponseContent) GetStatusOk() (*string, bool)`

GetStatusOk returns a tuple with the Status field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetStatus

`func (o *UpdateOrganisationResponseContent) SetStatus(v string)`

SetStatus sets Status field to given value.


### GetSector

`func (o *UpdateOrganisationResponseContent) GetSector() string`

GetSector returns the Sector field if non-nil, zero value otherwise.

### GetSectorOk

`func (o *UpdateOrganisationResponseContent) GetSectorOk() (*string, bool)`

GetSectorOk returns a tuple with the Sector field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetSector

`func (o *UpdateOrganisationResponseContent) SetSector(v string)`

SetSector sets Sector field to given value.

### HasSector

`func (o *UpdateOrganisationResponseContent) HasSector() bool`

HasSector returns a boolean if a field has been set.

### GetCreatedAt

`func (o *UpdateOrganisationResponseContent) GetCreatedAt() time.Time`

GetCreatedAt returns the CreatedAt field if non-nil, zero value otherwise.

### GetCreatedAtOk

`func (o *UpdateOrganisationResponseContent) GetCreatedAtOk() (*time.Time, bool)`

GetCreatedAtOk returns a tuple with the CreatedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreatedAt

`func (o *UpdateOrganisationResponseContent) SetCreatedAt(v time.Time)`

SetCreatedAt sets CreatedAt field to given value.


### GetUpdatedAt

`func (o *UpdateOrganisationResponseContent) GetUpdatedAt() time.Time`

GetUpdatedAt returns the UpdatedAt field if non-nil, zero value otherwise.

### GetUpdatedAtOk

`func (o *UpdateOrganisationResponseContent) GetUpdatedAtOk() (*time.Time, bool)`

GetUpdatedAtOk returns a tuple with the UpdatedAt field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetUpdatedAt

`func (o *UpdateOrganisationResponseContent) SetUpdatedAt(v time.Time)`

SetUpdatedAt sets UpdatedAt field to given value.


### GetUpdatedBy

`func (o *UpdateOrganisationResponseContent) GetUpdatedBy() string`

GetUpdatedBy returns the UpdatedBy field if non-nil, zero value otherwise.

### GetUpdatedByOk

`func (o *UpdateOrganisationResponseContent) GetUpdatedByOk() (*string, bool)`

GetUpdatedByOk returns a tuple with the UpdatedBy field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetUpdatedBy

`func (o *UpdateOrganisationResponseContent) SetUpdatedBy(v string)`

SetUpdatedBy sets UpdatedBy field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


