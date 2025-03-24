# ListDefaultConfigsResponseContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**TotalPages** | Pointer to **float32** |  | [optional] 
**TotalItems** | Pointer to **float32** |  | [optional] 
**Data** | Pointer to [**[]DefaultConfigFull**](DefaultConfigFull.md) |  | [optional] 

## Methods

### NewListDefaultConfigsResponseContent

`func NewListDefaultConfigsResponseContent() *ListDefaultConfigsResponseContent`

NewListDefaultConfigsResponseContent instantiates a new ListDefaultConfigsResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewListDefaultConfigsResponseContentWithDefaults

`func NewListDefaultConfigsResponseContentWithDefaults() *ListDefaultConfigsResponseContent`

NewListDefaultConfigsResponseContentWithDefaults instantiates a new ListDefaultConfigsResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetTotalPages

`func (o *ListDefaultConfigsResponseContent) GetTotalPages() float32`

GetTotalPages returns the TotalPages field if non-nil, zero value otherwise.

### GetTotalPagesOk

`func (o *ListDefaultConfigsResponseContent) GetTotalPagesOk() (*float32, bool)`

GetTotalPagesOk returns a tuple with the TotalPages field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTotalPages

`func (o *ListDefaultConfigsResponseContent) SetTotalPages(v float32)`

SetTotalPages sets TotalPages field to given value.

### HasTotalPages

`func (o *ListDefaultConfigsResponseContent) HasTotalPages() bool`

HasTotalPages returns a boolean if a field has been set.

### GetTotalItems

`func (o *ListDefaultConfigsResponseContent) GetTotalItems() float32`

GetTotalItems returns the TotalItems field if non-nil, zero value otherwise.

### GetTotalItemsOk

`func (o *ListDefaultConfigsResponseContent) GetTotalItemsOk() (*float32, bool)`

GetTotalItemsOk returns a tuple with the TotalItems field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTotalItems

`func (o *ListDefaultConfigsResponseContent) SetTotalItems(v float32)`

SetTotalItems sets TotalItems field to given value.

### HasTotalItems

`func (o *ListDefaultConfigsResponseContent) HasTotalItems() bool`

HasTotalItems returns a boolean if a field has been set.

### GetData

`func (o *ListDefaultConfigsResponseContent) GetData() []DefaultConfigFull`

GetData returns the Data field if non-nil, zero value otherwise.

### GetDataOk

`func (o *ListDefaultConfigsResponseContent) GetDataOk() (*[]DefaultConfigFull, bool)`

GetDataOk returns a tuple with the Data field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetData

`func (o *ListDefaultConfigsResponseContent) SetData(v []DefaultConfigFull)`

SetData sets Data field to given value.

### HasData

`func (o *ListDefaultConfigsResponseContent) HasData() bool`

HasData returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


