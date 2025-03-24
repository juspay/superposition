# GetTypeTemplatesListResponseContent

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**TotalPages** | **float32** |  | 
**TotalItems** | **float32** |  | 
**Data** | [**[]TypeTemplatesResponse**](TypeTemplatesResponse.md) |  | 

## Methods

### NewGetTypeTemplatesListResponseContent

`func NewGetTypeTemplatesListResponseContent(totalPages float32, totalItems float32, data []TypeTemplatesResponse, ) *GetTypeTemplatesListResponseContent`

NewGetTypeTemplatesListResponseContent instantiates a new GetTypeTemplatesListResponseContent object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewGetTypeTemplatesListResponseContentWithDefaults

`func NewGetTypeTemplatesListResponseContentWithDefaults() *GetTypeTemplatesListResponseContent`

NewGetTypeTemplatesListResponseContentWithDefaults instantiates a new GetTypeTemplatesListResponseContent object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetTotalPages

`func (o *GetTypeTemplatesListResponseContent) GetTotalPages() float32`

GetTotalPages returns the TotalPages field if non-nil, zero value otherwise.

### GetTotalPagesOk

`func (o *GetTypeTemplatesListResponseContent) GetTotalPagesOk() (*float32, bool)`

GetTotalPagesOk returns a tuple with the TotalPages field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTotalPages

`func (o *GetTypeTemplatesListResponseContent) SetTotalPages(v float32)`

SetTotalPages sets TotalPages field to given value.


### GetTotalItems

`func (o *GetTypeTemplatesListResponseContent) GetTotalItems() float32`

GetTotalItems returns the TotalItems field if non-nil, zero value otherwise.

### GetTotalItemsOk

`func (o *GetTypeTemplatesListResponseContent) GetTotalItemsOk() (*float32, bool)`

GetTotalItemsOk returns a tuple with the TotalItems field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTotalItems

`func (o *GetTypeTemplatesListResponseContent) SetTotalItems(v float32)`

SetTotalItems sets TotalItems field to given value.


### GetData

`func (o *GetTypeTemplatesListResponseContent) GetData() []TypeTemplatesResponse`

GetData returns the Data field if non-nil, zero value otherwise.

### GetDataOk

`func (o *GetTypeTemplatesListResponseContent) GetDataOk() (*[]TypeTemplatesResponse, bool)`

GetDataOk returns a tuple with the Data field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetData

`func (o *GetTypeTemplatesListResponseContent) SetData(v []TypeTemplatesResponse)`

SetData sets Data field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


