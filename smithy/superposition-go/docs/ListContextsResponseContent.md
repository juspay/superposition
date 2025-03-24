# ListContextsResponseContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**TotalPages** | Pointer to **float32** |  | [optional] 
**TotalItems** | Pointer to **float32** |  | [optional] 
**Data** | Pointer to [**[]ContextFull**](ContextFull.md) |  | [optional] 

## Methods

### NewListContextsResponseContent

`func NewListContextsResponseContent() *ListContextsResponseContent`

NewListContextsResponseContent instantiates a new ListContextsResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewListContextsResponseContentWithDefaults

`func NewListContextsResponseContentWithDefaults() *ListContextsResponseContent`

NewListContextsResponseContentWithDefaults instantiates a new ListContextsResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetTotalPages

`func (o *ListContextsResponseContent) GetTotalPages() float32`

GetTotalPages returns the TotalPages field if non-nil, zero value otherwise.

### GetTotalPagesOk

`func (o *ListContextsResponseContent) GetTotalPagesOk() (*float32, bool)`

GetTotalPagesOk returns a tuple with the TotalPages field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTotalPages

`func (o *ListContextsResponseContent) SetTotalPages(v float32)`

SetTotalPages sets TotalPages field to given value.

### HasTotalPages

`func (o *ListContextsResponseContent) HasTotalPages() bool`

HasTotalPages returns a boolean if a field has been set.

### GetTotalItems

`func (o *ListContextsResponseContent) GetTotalItems() float32`

GetTotalItems returns the TotalItems field if non-nil, zero value otherwise.

### GetTotalItemsOk

`func (o *ListContextsResponseContent) GetTotalItemsOk() (*float32, bool)`

GetTotalItemsOk returns a tuple with the TotalItems field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTotalItems

`func (o *ListContextsResponseContent) SetTotalItems(v float32)`

SetTotalItems sets TotalItems field to given value.

### HasTotalItems

`func (o *ListContextsResponseContent) HasTotalItems() bool`

HasTotalItems returns a boolean if a field has been set.

### GetData

`func (o *ListContextsResponseContent) GetData() []ContextFull`

GetData returns the Data field if non-nil, zero value otherwise.

### GetDataOk

`func (o *ListContextsResponseContent) GetDataOk() (*[]ContextFull, bool)`

GetDataOk returns a tuple with the Data field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetData

`func (o *ListContextsResponseContent) SetData(v []ContextFull)`

SetData sets Data field to given value.

### HasData

`func (o *ListContextsResponseContent) HasData() bool`

HasData returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


