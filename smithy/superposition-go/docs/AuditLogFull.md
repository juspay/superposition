# AuditLogFull

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**TableName** | Pointer to **string** |  | [optional] 
**UserName** | Pointer to **string** |  | [optional] 
**Timestamp** | Pointer to **time.Time** |  | [optional] 
**Action** | Pointer to **string** |  | [optional] 
**OriginalData** | Pointer to **interface{}** |  | [optional] 
**NewData** | Pointer to **interface{}** |  | [optional] 
**Query** | Pointer to **string** |  | [optional] 

## Methods

### NewAuditLogFull

`func NewAuditLogFull() *AuditLogFull`

NewAuditLogFull instantiates a new AuditLogFull object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewAuditLogFullWithDefaults

`func NewAuditLogFullWithDefaults() *AuditLogFull`

NewAuditLogFullWithDefaults instantiates a new AuditLogFull object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetTableName

`func (o *AuditLogFull) GetTableName() string`

GetTableName returns the TableName field if non-nil, zero value otherwise.

### GetTableNameOk

`func (o *AuditLogFull) GetTableNameOk() (*string, bool)`

GetTableNameOk returns a tuple with the TableName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTableName

`func (o *AuditLogFull) SetTableName(v string)`

SetTableName sets TableName field to given value.

### HasTableName

`func (o *AuditLogFull) HasTableName() bool`

HasTableName returns a boolean if a field has been set.

### GetUserName

`func (o *AuditLogFull) GetUserName() string`

GetUserName returns the UserName field if non-nil, zero value otherwise.

### GetUserNameOk

`func (o *AuditLogFull) GetUserNameOk() (*string, bool)`

GetUserNameOk returns a tuple with the UserName field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetUserName

`func (o *AuditLogFull) SetUserName(v string)`

SetUserName sets UserName field to given value.

### HasUserName

`func (o *AuditLogFull) HasUserName() bool`

HasUserName returns a boolean if a field has been set.

### GetTimestamp

`func (o *AuditLogFull) GetTimestamp() time.Time`

GetTimestamp returns the Timestamp field if non-nil, zero value otherwise.

### GetTimestampOk

`func (o *AuditLogFull) GetTimestampOk() (*time.Time, bool)`

GetTimestampOk returns a tuple with the Timestamp field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTimestamp

`func (o *AuditLogFull) SetTimestamp(v time.Time)`

SetTimestamp sets Timestamp field to given value.

### HasTimestamp

`func (o *AuditLogFull) HasTimestamp() bool`

HasTimestamp returns a boolean if a field has been set.

### GetAction

`func (o *AuditLogFull) GetAction() string`

GetAction returns the Action field if non-nil, zero value otherwise.

### GetActionOk

`func (o *AuditLogFull) GetActionOk() (*string, bool)`

GetActionOk returns a tuple with the Action field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetAction

`func (o *AuditLogFull) SetAction(v string)`

SetAction sets Action field to given value.

### HasAction

`func (o *AuditLogFull) HasAction() bool`

HasAction returns a boolean if a field has been set.

### GetOriginalData

`func (o *AuditLogFull) GetOriginalData() interface{}`

GetOriginalData returns the OriginalData field if non-nil, zero value otherwise.

### GetOriginalDataOk

`func (o *AuditLogFull) GetOriginalDataOk() (*interface{}, bool)`

GetOriginalDataOk returns a tuple with the OriginalData field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetOriginalData

`func (o *AuditLogFull) SetOriginalData(v interface{})`

SetOriginalData sets OriginalData field to given value.

### HasOriginalData

`func (o *AuditLogFull) HasOriginalData() bool`

HasOriginalData returns a boolean if a field has been set.

### SetOriginalDataNil

`func (o *AuditLogFull) SetOriginalDataNil(b bool)`

 SetOriginalDataNil sets the value for OriginalData to be an explicit nil

### UnsetOriginalData
`func (o *AuditLogFull) UnsetOriginalData()`

UnsetOriginalData ensures that no value is present for OriginalData, not even an explicit nil
### GetNewData

`func (o *AuditLogFull) GetNewData() interface{}`

GetNewData returns the NewData field if non-nil, zero value otherwise.

### GetNewDataOk

`func (o *AuditLogFull) GetNewDataOk() (*interface{}, bool)`

GetNewDataOk returns a tuple with the NewData field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetNewData

`func (o *AuditLogFull) SetNewData(v interface{})`

SetNewData sets NewData field to given value.

### HasNewData

`func (o *AuditLogFull) HasNewData() bool`

HasNewData returns a boolean if a field has been set.

### SetNewDataNil

`func (o *AuditLogFull) SetNewDataNil(b bool)`

 SetNewDataNil sets the value for NewData to be an explicit nil

### UnsetNewData
`func (o *AuditLogFull) UnsetNewData()`

UnsetNewData ensures that no value is present for NewData, not even an explicit nil
### GetQuery

`func (o *AuditLogFull) GetQuery() string`

GetQuery returns the Query field if non-nil, zero value otherwise.

### GetQueryOk

`func (o *AuditLogFull) GetQueryOk() (*string, bool)`

GetQueryOk returns a tuple with the Query field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetQuery

`func (o *AuditLogFull) SetQuery(v string)`

SetQuery sets Query field to given value.

### HasQuery

`func (o *AuditLogFull) HasQuery() bool`

HasQuery returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


