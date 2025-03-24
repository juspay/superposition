# ExperimentListResponse

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**TotalPages** | **float32** |  | 
**TotalItems** | **float32** |  | 
**Data** | [**[]ExperimentResponse**](ExperimentResponse.md) |  | 

## Methods

### NewExperimentListResponse

`func NewExperimentListResponse(totalPages float32, totalItems float32, data []ExperimentResponse, ) *ExperimentListResponse`

NewExperimentListResponse instantiates a new ExperimentListResponse object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewExperimentListResponseWithDefaults

`func NewExperimentListResponseWithDefaults() *ExperimentListResponse`

NewExperimentListResponseWithDefaults instantiates a new ExperimentListResponse object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetTotalPages

`func (o *ExperimentListResponse) GetTotalPages() float32`

GetTotalPages returns the TotalPages field if non-nil, zero value otherwise.

### GetTotalPagesOk

`func (o *ExperimentListResponse) GetTotalPagesOk() (*float32, bool)`

GetTotalPagesOk returns a tuple with the TotalPages field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTotalPages

`func (o *ExperimentListResponse) SetTotalPages(v float32)`

SetTotalPages sets TotalPages field to given value.


### GetTotalItems

`func (o *ExperimentListResponse) GetTotalItems() float32`

GetTotalItems returns the TotalItems field if non-nil, zero value otherwise.

### GetTotalItemsOk

`func (o *ExperimentListResponse) GetTotalItemsOk() (*float32, bool)`

GetTotalItemsOk returns a tuple with the TotalItems field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTotalItems

`func (o *ExperimentListResponse) SetTotalItems(v float32)`

SetTotalItems sets TotalItems field to given value.


### GetData

`func (o *ExperimentListResponse) GetData() []ExperimentResponse`

GetData returns the Data field if non-nil, zero value otherwise.

### GetDataOk

`func (o *ExperimentListResponse) GetDataOk() (*[]ExperimentResponse, bool)`

GetDataOk returns a tuple with the Data field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetData

`func (o *ExperimentListResponse) SetData(v []ExperimentResponse)`

SetData sets Data field to given value.



[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


