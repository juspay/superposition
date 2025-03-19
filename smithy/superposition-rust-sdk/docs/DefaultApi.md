# \DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**conclude_experiment**](DefaultApi.md#conclude_experiment) | **PATCH** /experiments/{id}/conclude | 
[**create_context**](DefaultApi.md#create_context) | **PUT** /context/ | 
[**create_default_config**](DefaultApi.md#create_default_config) | **POST** /default-config | 
[**create_dimension**](DefaultApi.md#create_dimension) | **POST** /dimension | 
[**create_experiment**](DefaultApi.md#create_experiment) | **POST** /experiments | 
[**create_function**](DefaultApi.md#create_function) | **POST** /function | 
[**create_type_templates**](DefaultApi.md#create_type_templates) | **POST** /types | 
[**create_workspace**](DefaultApi.md#create_workspace) | **POST** /workspaces | 
[**creater_organisation**](DefaultApi.md#creater_organisation) | **POST** /superposition/organisations | 
[**delete_default_config**](DefaultApi.md#delete_default_config) | **DELETE** /default-config/{key} | 
[**delete_dimension**](DefaultApi.md#delete_dimension) | **DELETE** /dimension/{dimension} | 
[**delete_function**](DefaultApi.md#delete_function) | **DELETE** /function/{function_name} | 
[**delete_type_templates**](DefaultApi.md#delete_type_templates) | **DELETE** /types/{type_name} | 
[**discard_experiment**](DefaultApi.md#discard_experiment) | **PATCH** /experiments/{id}/discard | 
[**get_context**](DefaultApi.md#get_context) | **GET** /context/{id} | 
[**get_context_from_condition**](DefaultApi.md#get_context_from_condition) | **POST** /context/get | 
[**get_experiment**](DefaultApi.md#get_experiment) | **GET** /experiments/{id} | 
[**get_function**](DefaultApi.md#get_function) | **GET** /function/{function_name} | 
[**get_organisation**](DefaultApi.md#get_organisation) | **GET** /superposition/organisations/{id} | 
[**get_type_templates_list**](DefaultApi.md#get_type_templates_list) | **GET** /types | 
[**list_contexts**](DefaultApi.md#list_contexts) | **GET** /context/list | 
[**list_default_configs**](DefaultApi.md#list_default_configs) | **GET** /default-config | 
[**list_dimensions**](DefaultApi.md#list_dimensions) | **GET** /dimension | 
[**list_experiment**](DefaultApi.md#list_experiment) | **GET** /experiments | 
[**list_organisation**](DefaultApi.md#list_organisation) | **GET** /superposition/organisations | 
[**list_workspace**](DefaultApi.md#list_workspace) | **GET** /workspaces | 
[**move_context**](DefaultApi.md#move_context) | **PUT** /context/move/{id} | 
[**publish**](DefaultApi.md#publish) | **PUT** /function/{function_name}/publish | 
[**ramp_experiment**](DefaultApi.md#ramp_experiment) | **PATCH** /experiments/{id}/ramp | 
[**test**](DefaultApi.md#test) | **PUT** /function/{function_name}/{stage}/test | 
[**update_default_config**](DefaultApi.md#update_default_config) | **PUT** /default-config/{key} | 
[**update_dimension**](DefaultApi.md#update_dimension) | **PUT** /dimension/{dimension} | 
[**update_function**](DefaultApi.md#update_function) | **PATCH** /function/{function_name} | 
[**update_organisation**](DefaultApi.md#update_organisation) | **PUT** /superposition/organisations/{id} | 
[**update_override**](DefaultApi.md#update_override) | **PUT** /context/overrides | 
[**update_overrides_experiment**](DefaultApi.md#update_overrides_experiment) | **PUT** /experiments/{id}/overrides | 
[**update_type_templates**](DefaultApi.md#update_type_templates) | **PUT** /types/{type_name} | 
[**update_workspace**](DefaultApi.md#update_workspace) | **PUT** /workspaces/{workspace_name} | 



## conclude_experiment

> models::ConcludeExperimentResponseContent conclude_experiment(id, x_org_id, x_tenant, conclude_experiment_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**conclude_experiment_request_content** | [**ConcludeExperimentRequestContent**](ConcludeExperimentRequestContent.md) |  | [required] |

### Return type

[**models::ConcludeExperimentResponseContent**](ConcludeExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## create_context

> models::CreateContextResponseContent create_context(x_org_id, x_tenant, create_context_request_content, x_config_tags)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**create_context_request_content** | [**CreateContextRequestContent**](CreateContextRequestContent.md) |  | [required] |
**x_config_tags** | Option<**String**> |  |  |

### Return type

[**models::CreateContextResponseContent**](CreateContextResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## create_default_config

> models::CreateDefaultConfigResponseContent create_default_config(x_org_id, x_tenant, create_default_config_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**create_default_config_request_content** | [**CreateDefaultConfigRequestContent**](CreateDefaultConfigRequestContent.md) |  | [required] |

### Return type

[**models::CreateDefaultConfigResponseContent**](CreateDefaultConfigResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## create_dimension

> models::CreateDimensionResponseContent create_dimension(x_org_id, x_tenant, create_dimension_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**create_dimension_request_content** | [**CreateDimensionRequestContent**](CreateDimensionRequestContent.md) |  | [required] |

### Return type

[**models::CreateDimensionResponseContent**](CreateDimensionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## create_experiment

> models::CreateExperimentResponseContent create_experiment(x_org_id, x_tenant, create_experiment_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**create_experiment_request_content** | [**CreateExperimentRequestContent**](CreateExperimentRequestContent.md) |  | [required] |

### Return type

[**models::CreateExperimentResponseContent**](CreateExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## create_function

> models::CreateFunctionResponseContent create_function(x_org_id, x_tenant, create_function_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**create_function_request_content** | [**CreateFunctionRequestContent**](CreateFunctionRequestContent.md) |  | [required] |

### Return type

[**models::CreateFunctionResponseContent**](CreateFunctionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## create_type_templates

> models::CreateTypeTemplatesResponseContent create_type_templates(x_org_id, x_tenant, create_type_templates_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**create_type_templates_request_content** | [**CreateTypeTemplatesRequestContent**](CreateTypeTemplatesRequestContent.md) |  | [required] |

### Return type

[**models::CreateTypeTemplatesResponseContent**](CreateTypeTemplatesResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## create_workspace

> models::CreateWorkspaceResponseContent create_workspace(x_org_id, x_tenant, create_workspace_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**create_workspace_request_content** | [**CreateWorkspaceRequestContent**](CreateWorkspaceRequestContent.md) |  | [required] |

### Return type

[**models::CreateWorkspaceResponseContent**](CreateWorkspaceResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## creater_organisation

> models::CreaterOrganisationResponseContent creater_organisation(x_org_id, x_tenant, creater_organisation_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**creater_organisation_request_content** | [**CreaterOrganisationRequestContent**](CreaterOrganisationRequestContent.md) |  | [required] |

### Return type

[**models::CreaterOrganisationResponseContent**](CreaterOrganisationResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delete_default_config

> delete_default_config(key, x_org_id, x_tenant)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |

### Return type

 (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delete_dimension

> delete_dimension(dimension, x_org_id, x_tenant)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**dimension** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |

### Return type

 (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delete_function

> delete_function(function_name, x_org_id, x_tenant)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**function_name** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |

### Return type

 (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delete_type_templates

> delete_type_templates(type_name, x_org_id, x_tenant)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**type_name** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |

### Return type

 (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## discard_experiment

> models::DiscardExperimentResponseContent discard_experiment(id, x_org_id, x_tenant, discard_experiment_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**discard_experiment_request_content** | [**DiscardExperimentRequestContent**](DiscardExperimentRequestContent.md) |  | [required] |

### Return type

[**models::DiscardExperimentResponseContent**](DiscardExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_context

> models::GetContextResponseContent get_context(id, x_org_id, x_tenant)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |

### Return type

[**models::GetContextResponseContent**](GetContextResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_context_from_condition

> models::GetContextFromConditionResponseContent get_context_from_condition(x_org_id, x_tenant, body)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**body** | Option<**serde_json::Value**> |  |  |

### Return type

[**models::GetContextFromConditionResponseContent**](GetContextFromConditionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_experiment

> models::GetExperimentResponseContent get_experiment(id, x_org_id, x_tenant)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |

### Return type

[**models::GetExperimentResponseContent**](GetExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_function

> models::GetFunctionResponseContent get_function(function_name, x_org_id, x_tenant)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**function_name** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |

### Return type

[**models::GetFunctionResponseContent**](GetFunctionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_organisation

> models::GetOrganisationResponseContent get_organisation(id, x_org_id, x_tenant)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |

### Return type

[**models::GetOrganisationResponseContent**](GetOrganisationResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_type_templates_list

> models::GetTypeTemplatesListResponseContent get_type_templates_list(x_org_id, x_tenant, page, count, all)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**page** | Option<**f64**> |  |  |
**count** | Option<**f64**> |  |  |
**all** | Option<**bool**> |  |  |

### Return type

[**models::GetTypeTemplatesListResponseContent**](GetTypeTemplatesListResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## list_contexts

> models::ListContextsResponseContent list_contexts(x_org_id, x_tenant, page, size, prefix, sort_on, sort_by, created_by)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**page** | Option<**f64**> |  |  |
**size** | Option<**f64**> |  |  |
**prefix** | Option<**String**> |  |  |
**sort_on** | Option<[**ContextFilterSortOn**](.md)> |  |  |
**sort_by** | Option<[**SortBy**](.md)> |  |  |
**created_by** | Option<**String**> |  |  |

### Return type

[**models::ListContextsResponseContent**](ListContextsResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## list_default_configs

> models::ListDefaultConfigsResponseContent list_default_configs(x_org_id, x_tenant, count, page)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**count** | Option<**f64**> |  |  |
**page** | Option<**f64**> |  |  |

### Return type

[**models::ListDefaultConfigsResponseContent**](ListDefaultConfigsResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## list_dimensions

> models::ListDimensionsResponseContent list_dimensions(x_org_id, x_tenant, count, page)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**count** | Option<**f64**> |  |  |
**page** | Option<**f64**> |  |  |

### Return type

[**models::ListDimensionsResponseContent**](ListDimensionsResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## list_experiment

> models::ListExperimentResponseContent list_experiment(x_org_id, x_tenant, page, count, all)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**page** | Option<**f64**> |  |  |
**count** | Option<**f64**> |  |  |
**all** | Option<**bool**> |  |  |

### Return type

[**models::ListExperimentResponseContent**](ListExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## list_organisation

> models::ListOrganisationResponseContent list_organisation(x_org_id, x_tenant, page, count, all)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**page** | Option<**f64**> |  |  |
**count** | Option<**f64**> |  |  |
**all** | Option<**bool**> |  |  |

### Return type

[**models::ListOrganisationResponseContent**](ListOrganisationResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## list_workspace

> models::ListWorkspaceResponseContent list_workspace(x_org_id, x_tenant, page, count, all)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**page** | Option<**f64**> |  |  |
**count** | Option<**f64**> |  |  |
**all** | Option<**bool**> |  |  |

### Return type

[**models::ListWorkspaceResponseContent**](ListWorkspaceResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## move_context

> models::MoveContextResponseContent move_context(id, x_org_id, x_tenant, move_context_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**move_context_request_content** | [**MoveContextRequestContent**](MoveContextRequestContent.md) |  | [required] |

### Return type

[**models::MoveContextResponseContent**](MoveContextResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## publish

> models::PublishResponseContent publish(function_name, x_org_id, x_tenant)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**function_name** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |

### Return type

[**models::PublishResponseContent**](PublishResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## ramp_experiment

> models::RampExperimentResponseContent ramp_experiment(id, x_org_id, x_tenant, ramp_experiment_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**ramp_experiment_request_content** | [**RampExperimentRequestContent**](RampExperimentRequestContent.md) |  | [required] |

### Return type

[**models::RampExperimentResponseContent**](RampExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## test

> test(function_name, stage, x_org_id, x_tenant)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**function_name** | **String** |  | [required] |
**stage** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |

### Return type

 (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_default_config

> models::UpdateDefaultConfigResponseContent update_default_config(key, x_org_id, x_tenant, update_default_config_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**update_default_config_request_content** | [**UpdateDefaultConfigRequestContent**](UpdateDefaultConfigRequestContent.md) |  | [required] |

### Return type

[**models::UpdateDefaultConfigResponseContent**](UpdateDefaultConfigResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_dimension

> models::UpdateDimensionResponseContent update_dimension(dimension, x_org_id, x_tenant, update_dimension_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**dimension** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**update_dimension_request_content** | [**UpdateDimensionRequestContent**](UpdateDimensionRequestContent.md) |  | [required] |

### Return type

[**models::UpdateDimensionResponseContent**](UpdateDimensionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_function

> models::UpdateFunctionResponseContent update_function(function_name, x_org_id, x_tenant, update_function_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**function_name** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**update_function_request_content** | [**UpdateFunctionRequestContent**](UpdateFunctionRequestContent.md) |  | [required] |

### Return type

[**models::UpdateFunctionResponseContent**](UpdateFunctionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_organisation

> models::UpdateOrganisationResponseContent update_organisation(id, x_org_id, x_tenant, update_organisation_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**update_organisation_request_content** | Option<[**UpdateOrganisationRequestContent**](UpdateOrganisationRequestContent.md)> |  |  |

### Return type

[**models::UpdateOrganisationResponseContent**](UpdateOrganisationResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_override

> models::UpdateOverrideResponseContent update_override(x_org_id, x_tenant, update_override_request_content, x_config_tags)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**update_override_request_content** | [**UpdateOverrideRequestContent**](UpdateOverrideRequestContent.md) |  | [required] |
**x_config_tags** | Option<**String**> |  |  |

### Return type

[**models::UpdateOverrideResponseContent**](UpdateOverrideResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_overrides_experiment

> models::UpdateOverridesExperimentResponseContent update_overrides_experiment(id, x_org_id, x_tenant, update_overrides_experiment_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**update_overrides_experiment_request_content** | [**UpdateOverridesExperimentRequestContent**](UpdateOverridesExperimentRequestContent.md) |  | [required] |

### Return type

[**models::UpdateOverridesExperimentResponseContent**](UpdateOverridesExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_type_templates

> models::UpdateTypeTemplatesResponseContent update_type_templates(type_name, x_org_id, x_tenant, update_type_templates_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**type_name** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**update_type_templates_request_content** | [**UpdateTypeTemplatesRequestContent**](UpdateTypeTemplatesRequestContent.md) |  | [required] |

### Return type

[**models::UpdateTypeTemplatesResponseContent**](UpdateTypeTemplatesResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_workspace

> models::UpdateWorkspaceResponseContent update_workspace(workspace_name, x_org_id, x_tenant, update_workspace_request_content)


### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**workspace_name** | **String** |  | [required] |
**x_org_id** | **String** |  | [required] |[default to juspay]
**x_tenant** | **String** |  | [required] |
**update_workspace_request_content** | [**UpdateWorkspaceRequestContent**](UpdateWorkspaceRequestContent.md) |  | [required] |

### Return type

[**models::UpdateWorkspaceResponseContent**](UpdateWorkspaceResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

