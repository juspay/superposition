# superposition_sdk_python.DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**conclude_experiment**](DefaultApi.md#conclude_experiment) | **PATCH** /experiments/{id}/conclude | 
[**create_context**](DefaultApi.md#create_context) | **PUT** /context | 
[**create_default_config**](DefaultApi.md#create_default_config) | **POST** /default-config | 
[**create_dimension**](DefaultApi.md#create_dimension) | **POST** /dimension | 
[**create_experiment**](DefaultApi.md#create_experiment) | **POST** /experiments | 
[**create_function**](DefaultApi.md#create_function) | **POST** /function | 
[**create_type_templates**](DefaultApi.md#create_type_templates) | **POST** /types | 
[**create_workspace**](DefaultApi.md#create_workspace) | **POST** /workspaces | 
[**creater_organisation**](DefaultApi.md#creater_organisation) | **POST** /superposition/organisations | 
[**delete_context**](DefaultApi.md#delete_context) | **DELETE** /context/{id} | 
[**delete_default_config**](DefaultApi.md#delete_default_config) | **DELETE** /default-config/{key} | 
[**delete_dimension**](DefaultApi.md#delete_dimension) | **DELETE** /dimension/{dimension} | 
[**delete_function**](DefaultApi.md#delete_function) | **DELETE** /function/{function_name} | 
[**delete_type_templates**](DefaultApi.md#delete_type_templates) | **DELETE** /types/{type_name} | 
[**discard_experiment**](DefaultApi.md#discard_experiment) | **PATCH** /experiments/{id}/discard | 
[**get_config_fast**](DefaultApi.md#get_config_fast) | **GET** /config/fast | 
[**get_context**](DefaultApi.md#get_context) | **GET** /context/{id} | 
[**get_context_from_condition**](DefaultApi.md#get_context_from_condition) | **POST** /context/get | 
[**get_experiment**](DefaultApi.md#get_experiment) | **GET** /experiments/{id} | 
[**get_function**](DefaultApi.md#get_function) | **GET** /function/{function_name} | 
[**get_organisation**](DefaultApi.md#get_organisation) | **GET** /superposition/organisations/{id} | 
[**get_type_templates_list**](DefaultApi.md#get_type_templates_list) | **GET** /types | 
[**list_audit_logs**](DefaultApi.md#list_audit_logs) | **GET** /audit | 
[**list_contexts**](DefaultApi.md#list_contexts) | **GET** /context/list | 
[**list_default_configs**](DefaultApi.md#list_default_configs) | **GET** /default-config | 
[**list_dimensions**](DefaultApi.md#list_dimensions) | **GET** /dimension | 
[**list_experiment**](DefaultApi.md#list_experiment) | **GET** /experiments | 
[**list_organisation**](DefaultApi.md#list_organisation) | **GET** /superposition/organisations | 
[**list_versions**](DefaultApi.md#list_versions) | **GET** /config/versions | 
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
[**weight_recompute**](DefaultApi.md#weight_recompute) | **PUT** /weight/recompute | 


# **conclude_experiment**
> ConcludeExperimentResponseContent conclude_experiment(id, x_org_id, x_tenant, conclude_experiment_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.conclude_experiment_request_content import ConcludeExperimentRequestContent
from superposition_sdk_python.models.conclude_experiment_response_content import ConcludeExperimentResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    id = 'id_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    conclude_experiment_request_content = superposition_sdk_python.ConcludeExperimentRequestContent() # ConcludeExperimentRequestContent | 

    try:
        api_response = api_instance.conclude_experiment(id, x_org_id, x_tenant, conclude_experiment_request_content)
        print("The response of DefaultApi->conclude_experiment:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->conclude_experiment: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **conclude_experiment_request_content** | [**ConcludeExperimentRequestContent**](ConcludeExperimentRequestContent.md)|  | 

### Return type

[**ConcludeExperimentResponseContent**](ConcludeExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | ConcludeExperiment 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **create_context**
> CreateContextResponseContent create_context(x_org_id, x_tenant, create_context_request_content, x_config_tags=x_config_tags)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.create_context_request_content import CreateContextRequestContent
from superposition_sdk_python.models.create_context_response_content import CreateContextResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    create_context_request_content = superposition_sdk_python.CreateContextRequestContent() # CreateContextRequestContent | 
    x_config_tags = 'x_config_tags_example' # str |  (optional)

    try:
        api_response = api_instance.create_context(x_org_id, x_tenant, create_context_request_content, x_config_tags=x_config_tags)
        print("The response of DefaultApi->create_context:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->create_context: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **create_context_request_content** | [**CreateContextRequestContent**](CreateContextRequestContent.md)|  | 
 **x_config_tags** | **str**|  | [optional] 

### Return type

[**CreateContextResponseContent**](CreateContextResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | CreateContext 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **create_default_config**
> CreateDefaultConfigResponseContent create_default_config(x_org_id, x_tenant, create_default_config_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.create_default_config_request_content import CreateDefaultConfigRequestContent
from superposition_sdk_python.models.create_default_config_response_content import CreateDefaultConfigResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    create_default_config_request_content = superposition_sdk_python.CreateDefaultConfigRequestContent() # CreateDefaultConfigRequestContent | 

    try:
        api_response = api_instance.create_default_config(x_org_id, x_tenant, create_default_config_request_content)
        print("The response of DefaultApi->create_default_config:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->create_default_config: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **create_default_config_request_content** | [**CreateDefaultConfigRequestContent**](CreateDefaultConfigRequestContent.md)|  | 

### Return type

[**CreateDefaultConfigResponseContent**](CreateDefaultConfigResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | CreateDefaultConfig 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **create_dimension**
> CreateDimensionResponseContent create_dimension(x_org_id, x_tenant, create_dimension_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.create_dimension_request_content import CreateDimensionRequestContent
from superposition_sdk_python.models.create_dimension_response_content import CreateDimensionResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    create_dimension_request_content = superposition_sdk_python.CreateDimensionRequestContent() # CreateDimensionRequestContent | 

    try:
        api_response = api_instance.create_dimension(x_org_id, x_tenant, create_dimension_request_content)
        print("The response of DefaultApi->create_dimension:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->create_dimension: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **create_dimension_request_content** | [**CreateDimensionRequestContent**](CreateDimensionRequestContent.md)|  | 

### Return type

[**CreateDimensionResponseContent**](CreateDimensionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | CreateDimension 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **create_experiment**
> CreateExperimentResponseContent create_experiment(x_org_id, x_tenant, create_experiment_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.create_experiment_request_content import CreateExperimentRequestContent
from superposition_sdk_python.models.create_experiment_response_content import CreateExperimentResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    create_experiment_request_content = superposition_sdk_python.CreateExperimentRequestContent() # CreateExperimentRequestContent | 

    try:
        api_response = api_instance.create_experiment(x_org_id, x_tenant, create_experiment_request_content)
        print("The response of DefaultApi->create_experiment:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->create_experiment: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **create_experiment_request_content** | [**CreateExperimentRequestContent**](CreateExperimentRequestContent.md)|  | 

### Return type

[**CreateExperimentResponseContent**](CreateExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | CreateExperiment 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **create_function**
> CreateFunctionResponseContent create_function(x_org_id, x_tenant, create_function_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.create_function_request_content import CreateFunctionRequestContent
from superposition_sdk_python.models.create_function_response_content import CreateFunctionResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    create_function_request_content = superposition_sdk_python.CreateFunctionRequestContent() # CreateFunctionRequestContent | 

    try:
        api_response = api_instance.create_function(x_org_id, x_tenant, create_function_request_content)
        print("The response of DefaultApi->create_function:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->create_function: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **create_function_request_content** | [**CreateFunctionRequestContent**](CreateFunctionRequestContent.md)|  | 

### Return type

[**CreateFunctionResponseContent**](CreateFunctionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | CreateFunction 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **create_type_templates**
> CreateTypeTemplatesResponseContent create_type_templates(x_org_id, x_tenant, create_type_templates_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.create_type_templates_request_content import CreateTypeTemplatesRequestContent
from superposition_sdk_python.models.create_type_templates_response_content import CreateTypeTemplatesResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    create_type_templates_request_content = superposition_sdk_python.CreateTypeTemplatesRequestContent() # CreateTypeTemplatesRequestContent | 

    try:
        api_response = api_instance.create_type_templates(x_org_id, x_tenant, create_type_templates_request_content)
        print("The response of DefaultApi->create_type_templates:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->create_type_templates: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **create_type_templates_request_content** | [**CreateTypeTemplatesRequestContent**](CreateTypeTemplatesRequestContent.md)|  | 

### Return type

[**CreateTypeTemplatesResponseContent**](CreateTypeTemplatesResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | CreateTypeTemplates 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **create_workspace**
> CreateWorkspaceResponseContent create_workspace(x_org_id, create_workspace_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.create_workspace_request_content import CreateWorkspaceRequestContent
from superposition_sdk_python.models.create_workspace_response_content import CreateWorkspaceResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    create_workspace_request_content = superposition_sdk_python.CreateWorkspaceRequestContent() # CreateWorkspaceRequestContent | 

    try:
        api_response = api_instance.create_workspace(x_org_id, create_workspace_request_content)
        print("The response of DefaultApi->create_workspace:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->create_workspace: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **create_workspace_request_content** | [**CreateWorkspaceRequestContent**](CreateWorkspaceRequestContent.md)|  | 

### Return type

[**CreateWorkspaceResponseContent**](CreateWorkspaceResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | CreateWorkspace 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **creater_organisation**
> CreaterOrganisationResponseContent creater_organisation(creater_organisation_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.creater_organisation_request_content import CreaterOrganisationRequestContent
from superposition_sdk_python.models.creater_organisation_response_content import CreaterOrganisationResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    creater_organisation_request_content = superposition_sdk_python.CreaterOrganisationRequestContent() # CreaterOrganisationRequestContent | 

    try:
        api_response = api_instance.creater_organisation(creater_organisation_request_content)
        print("The response of DefaultApi->creater_organisation:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->creater_organisation: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **creater_organisation_request_content** | [**CreaterOrganisationRequestContent**](CreaterOrganisationRequestContent.md)|  | 

### Return type

[**CreaterOrganisationResponseContent**](CreaterOrganisationResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | CreaterOrganisation 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_context**
> delete_context(id, x_org_id, x_tenant, x_config_tags=x_config_tags)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    id = 'id_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    x_config_tags = 'x_config_tags_example' # str |  (optional)

    try:
        api_instance.delete_context(id, x_org_id, x_tenant, x_config_tags=x_config_tags)
    except Exception as e:
        print("Exception when calling DefaultApi->delete_context: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **x_config_tags** | **str**|  | [optional] 

### Return type

void (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**201** | DeleteContext 201 response |  -  |
**404** | ResourceNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_default_config**
> delete_default_config(key, x_org_id, x_tenant)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    key = 'key_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 

    try:
        api_instance.delete_default_config(key, x_org_id, x_tenant)
    except Exception as e:
        print("Exception when calling DefaultApi->delete_default_config: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **key** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 

### Return type

void (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**201** | DeleteDefaultConfig 201 response |  -  |
**404** | ResourceNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_dimension**
> delete_dimension(dimension, x_org_id, x_tenant)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    dimension = 'dimension_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 

    try:
        api_instance.delete_dimension(dimension, x_org_id, x_tenant)
    except Exception as e:
        print("Exception when calling DefaultApi->delete_dimension: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **dimension** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 

### Return type

void (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**201** | DeleteDimension 201 response |  -  |
**404** | ResourceNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_function**
> delete_function(function_name, x_org_id, x_tenant)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    function_name = 'function_name_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 

    try:
        api_instance.delete_function(function_name, x_org_id, x_tenant)
    except Exception as e:
        print("Exception when calling DefaultApi->delete_function: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **function_name** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 

### Return type

void (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | DeleteFunction 200 response |  -  |
**404** | FunctionNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_type_templates**
> delete_type_templates(type_name, x_org_id, x_tenant)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    type_name = 'type_name_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 

    try:
        api_instance.delete_type_templates(type_name, x_org_id, x_tenant)
    except Exception as e:
        print("Exception when calling DefaultApi->delete_type_templates: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **type_name** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 

### Return type

void (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | DeleteTypeTemplates 200 response |  -  |
**404** | TypeTemplatesNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **discard_experiment**
> DiscardExperimentResponseContent discard_experiment(id, x_org_id, x_tenant, discard_experiment_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.discard_experiment_request_content import DiscardExperimentRequestContent
from superposition_sdk_python.models.discard_experiment_response_content import DiscardExperimentResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    id = 'id_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    discard_experiment_request_content = superposition_sdk_python.DiscardExperimentRequestContent() # DiscardExperimentRequestContent | 

    try:
        api_response = api_instance.discard_experiment(id, x_org_id, x_tenant, discard_experiment_request_content)
        print("The response of DefaultApi->discard_experiment:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->discard_experiment: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **discard_experiment_request_content** | [**DiscardExperimentRequestContent**](DiscardExperimentRequestContent.md)|  | 

### Return type

[**DiscardExperimentResponseContent**](DiscardExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | DiscardExperiment 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_config_fast**
> object get_config_fast(x_org_id, x_tenant)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 

    try:
        api_response = api_instance.get_config_fast(x_org_id, x_tenant)
        print("The response of DefaultApi->get_config_fast:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->get_config_fast: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 

### Return type

**object**

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | GetConfigFast 200 response |  * last-modified -  <br>  * x-audit-id -  <br>  * x-config-version -  <br>  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_context**
> GetContextResponseContent get_context(id, x_org_id, x_tenant)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.get_context_response_content import GetContextResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    id = 'id_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 

    try:
        api_response = api_instance.get_context(id, x_org_id, x_tenant)
        print("The response of DefaultApi->get_context:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->get_context: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 

### Return type

[**GetContextResponseContent**](GetContextResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | GetContext 200 response |  -  |
**404** | ResourceNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_context_from_condition**
> GetContextFromConditionResponseContent get_context_from_condition(x_org_id, x_tenant, body=body)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.get_context_from_condition_response_content import GetContextFromConditionResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    body = None # object |  (optional)

    try:
        api_response = api_instance.get_context_from_condition(x_org_id, x_tenant, body=body)
        print("The response of DefaultApi->get_context_from_condition:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->get_context_from_condition: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **body** | **object**|  | [optional] 

### Return type

[**GetContextFromConditionResponseContent**](GetContextFromConditionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | GetContextFromCondition 200 response |  -  |
**404** | ResourceNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_experiment**
> GetExperimentResponseContent get_experiment(id, x_org_id, x_tenant)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.get_experiment_response_content import GetExperimentResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    id = 'id_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 

    try:
        api_response = api_instance.get_experiment(id, x_org_id, x_tenant)
        print("The response of DefaultApi->get_experiment:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->get_experiment: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 

### Return type

[**GetExperimentResponseContent**](GetExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | GetExperiment 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_function**
> GetFunctionResponseContent get_function(function_name, x_org_id, x_tenant)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.get_function_response_content import GetFunctionResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    function_name = 'function_name_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 

    try:
        api_response = api_instance.get_function(function_name, x_org_id, x_tenant)
        print("The response of DefaultApi->get_function:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->get_function: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **function_name** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 

### Return type

[**GetFunctionResponseContent**](GetFunctionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | GetFunction 200 response |  -  |
**404** | FunctionNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_organisation**
> GetOrganisationResponseContent get_organisation(id)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.get_organisation_response_content import GetOrganisationResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    id = 'id_example' # str | 

    try:
        api_response = api_instance.get_organisation(id)
        print("The response of DefaultApi->get_organisation:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->get_organisation: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **str**|  | 

### Return type

[**GetOrganisationResponseContent**](GetOrganisationResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | GetOrganisation 200 response |  -  |
**404** | OrganisationNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_type_templates_list**
> GetTypeTemplatesListResponseContent get_type_templates_list(x_org_id, x_tenant, page=page, count=count, all=all)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.get_type_templates_list_response_content import GetTypeTemplatesListResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    page = 3.4 # float |  (optional)
    count = 3.4 # float |  (optional)
    all = True # bool |  (optional)

    try:
        api_response = api_instance.get_type_templates_list(x_org_id, x_tenant, page=page, count=count, all=all)
        print("The response of DefaultApi->get_type_templates_list:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->get_type_templates_list: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **page** | **float**|  | [optional] 
 **count** | **float**|  | [optional] 
 **all** | **bool**|  | [optional] 

### Return type

[**GetTypeTemplatesListResponseContent**](GetTypeTemplatesListResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | GetTypeTemplatesList 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **list_audit_logs**
> ListAuditLogsResponseContent list_audit_logs(x_org_id, x_tenant, count=count, page=page, from_date=from_date, to_date=to_date, table=table, action=action, username=username)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.list_audit_logs_response_content import ListAuditLogsResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    count = 3.4 # float |  (optional)
    page = 3.4 # float |  (optional)
    from_date = '2013-10-20T19:20:30+01:00' # datetime |  (optional)
    to_date = '2013-10-20T19:20:30+01:00' # datetime |  (optional)
    table = 'table_example' # str | Comma serparated list of tables. (optional)
    action = 'action_example' # str | Comma serparated list of actions. (optional)
    username = 'username_example' # str |  (optional)

    try:
        api_response = api_instance.list_audit_logs(x_org_id, x_tenant, count=count, page=page, from_date=from_date, to_date=to_date, table=table, action=action, username=username)
        print("The response of DefaultApi->list_audit_logs:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->list_audit_logs: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **count** | **float**|  | [optional] 
 **page** | **float**|  | [optional] 
 **from_date** | **datetime**|  | [optional] 
 **to_date** | **datetime**|  | [optional] 
 **table** | **str**| Comma serparated list of tables. | [optional] 
 **action** | **str**| Comma serparated list of actions. | [optional] 
 **username** | **str**|  | [optional] 

### Return type

[**ListAuditLogsResponseContent**](ListAuditLogsResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | ListAuditLogs 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **list_contexts**
> ListContextsResponseContent list_contexts(x_org_id, x_tenant, page=page, size=size, prefix=prefix, sort_on=sort_on, sort_by=sort_by, created_by=created_by)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.context_filter_sort_on import ContextFilterSortOn
from superposition_sdk_python.models.list_contexts_response_content import ListContextsResponseContent
from superposition_sdk_python.models.sort_by import SortBy
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    page = 3.4 # float |  (optional)
    size = 3.4 # float |  (optional)
    prefix = 'prefix_example' # str |  (optional)
    sort_on = superposition_sdk_python.ContextFilterSortOn() # ContextFilterSortOn |  (optional)
    sort_by = superposition_sdk_python.SortBy() # SortBy |  (optional)
    created_by = 'created_by_example' # str |  (optional)

    try:
        api_response = api_instance.list_contexts(x_org_id, x_tenant, page=page, size=size, prefix=prefix, sort_on=sort_on, sort_by=sort_by, created_by=created_by)
        print("The response of DefaultApi->list_contexts:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->list_contexts: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **page** | **float**|  | [optional] 
 **size** | **float**|  | [optional] 
 **prefix** | **str**|  | [optional] 
 **sort_on** | [**ContextFilterSortOn**](.md)|  | [optional] 
 **sort_by** | [**SortBy**](.md)|  | [optional] 
 **created_by** | **str**|  | [optional] 

### Return type

[**ListContextsResponseContent**](ListContextsResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | ListContexts 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **list_default_configs**
> ListDefaultConfigsResponseContent list_default_configs(x_org_id, x_tenant, count=count, page=page)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.list_default_configs_response_content import ListDefaultConfigsResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    count = 3.4 # float |  (optional)
    page = 3.4 # float |  (optional)

    try:
        api_response = api_instance.list_default_configs(x_org_id, x_tenant, count=count, page=page)
        print("The response of DefaultApi->list_default_configs:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->list_default_configs: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **count** | **float**|  | [optional] 
 **page** | **float**|  | [optional] 

### Return type

[**ListDefaultConfigsResponseContent**](ListDefaultConfigsResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | ListDefaultConfigs 200 response |  -  |
**404** | ResourceNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **list_dimensions**
> ListDimensionsResponseContent list_dimensions(x_org_id, x_tenant, count=count, page=page)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.list_dimensions_response_content import ListDimensionsResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    count = 3.4 # float |  (optional)
    page = 3.4 # float |  (optional)

    try:
        api_response = api_instance.list_dimensions(x_org_id, x_tenant, count=count, page=page)
        print("The response of DefaultApi->list_dimensions:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->list_dimensions: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **count** | **float**|  | [optional] 
 **page** | **float**|  | [optional] 

### Return type

[**ListDimensionsResponseContent**](ListDimensionsResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | ListDimensions 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **list_experiment**
> ListExperimentResponseContent list_experiment(x_org_id, x_tenant, page=page, count=count, all=all)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.list_experiment_response_content import ListExperimentResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    page = 3.4 # float |  (optional)
    count = 3.4 # float |  (optional)
    all = True # bool |  (optional)

    try:
        api_response = api_instance.list_experiment(x_org_id, x_tenant, page=page, count=count, all=all)
        print("The response of DefaultApi->list_experiment:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->list_experiment: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **page** | **float**|  | [optional] 
 **count** | **float**|  | [optional] 
 **all** | **bool**|  | [optional] 

### Return type

[**ListExperimentResponseContent**](ListExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | ListExperiment 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **list_organisation**
> ListOrganisationResponseContent list_organisation(page=page, count=count, all=all)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.list_organisation_response_content import ListOrganisationResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    page = 3.4 # float |  (optional)
    count = 3.4 # float |  (optional)
    all = True # bool |  (optional)

    try:
        api_response = api_instance.list_organisation(page=page, count=count, all=all)
        print("The response of DefaultApi->list_organisation:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->list_organisation: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **page** | **float**|  | [optional] 
 **count** | **float**|  | [optional] 
 **all** | **bool**|  | [optional] 

### Return type

[**ListOrganisationResponseContent**](ListOrganisationResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | ListOrganisation 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **list_versions**
> ListVersionsResponseContent list_versions(x_org_id, x_tenant, count=count, page=page)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.list_versions_response_content import ListVersionsResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    count = 3.4 # float |  (optional)
    page = 3.4 # float |  (optional)

    try:
        api_response = api_instance.list_versions(x_org_id, x_tenant, count=count, page=page)
        print("The response of DefaultApi->list_versions:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->list_versions: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **count** | **float**|  | [optional] 
 **page** | **float**|  | [optional] 

### Return type

[**ListVersionsResponseContent**](ListVersionsResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | ListVersions 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **list_workspace**
> ListWorkspaceResponseContent list_workspace(x_org_id, page=page, count=count, all=all)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.list_workspace_response_content import ListWorkspaceResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    page = 3.4 # float |  (optional)
    count = 3.4 # float |  (optional)
    all = True # bool |  (optional)

    try:
        api_response = api_instance.list_workspace(x_org_id, page=page, count=count, all=all)
        print("The response of DefaultApi->list_workspace:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->list_workspace: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **page** | **float**|  | [optional] 
 **count** | **float**|  | [optional] 
 **all** | **bool**|  | [optional] 

### Return type

[**ListWorkspaceResponseContent**](ListWorkspaceResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | ListWorkspace 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **move_context**
> MoveContextResponseContent move_context(id, x_org_id, x_tenant, move_context_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.move_context_request_content import MoveContextRequestContent
from superposition_sdk_python.models.move_context_response_content import MoveContextResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    id = 'id_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    move_context_request_content = superposition_sdk_python.MoveContextRequestContent() # MoveContextRequestContent | 

    try:
        api_response = api_instance.move_context(id, x_org_id, x_tenant, move_context_request_content)
        print("The response of DefaultApi->move_context:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->move_context: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **move_context_request_content** | [**MoveContextRequestContent**](MoveContextRequestContent.md)|  | 

### Return type

[**MoveContextResponseContent**](MoveContextResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | MoveContext 200 response |  -  |
**404** | ResourceNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **publish**
> PublishResponseContent publish(function_name, x_org_id, x_tenant)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.publish_response_content import PublishResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    function_name = 'function_name_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 

    try:
        api_response = api_instance.publish(function_name, x_org_id, x_tenant)
        print("The response of DefaultApi->publish:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->publish: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **function_name** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 

### Return type

[**PublishResponseContent**](PublishResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Publish 200 response |  -  |
**404** | FunctionNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **ramp_experiment**
> RampExperimentResponseContent ramp_experiment(id, x_org_id, x_tenant, ramp_experiment_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.ramp_experiment_request_content import RampExperimentRequestContent
from superposition_sdk_python.models.ramp_experiment_response_content import RampExperimentResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    id = 'id_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    ramp_experiment_request_content = superposition_sdk_python.RampExperimentRequestContent() # RampExperimentRequestContent | 

    try:
        api_response = api_instance.ramp_experiment(id, x_org_id, x_tenant, ramp_experiment_request_content)
        print("The response of DefaultApi->ramp_experiment:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->ramp_experiment: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **ramp_experiment_request_content** | [**RampExperimentRequestContent**](RampExperimentRequestContent.md)|  | 

### Return type

[**RampExperimentResponseContent**](RampExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | RampExperiment 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test**
> test(function_name, stage, x_org_id, x_tenant)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.stage import Stage
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    function_name = 'function_name_example' # str | 
    stage = superposition_sdk_python.Stage() # Stage | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 

    try:
        api_instance.test(function_name, stage, x_org_id, x_tenant)
    except Exception as e:
        print("Exception when calling DefaultApi->test: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **function_name** | **str**|  | 
 **stage** | [**Stage**](.md)|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 

### Return type

void (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Test 200 response |  -  |
**404** | FunctionNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_default_config**
> UpdateDefaultConfigResponseContent update_default_config(key, x_org_id, x_tenant, update_default_config_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.update_default_config_request_content import UpdateDefaultConfigRequestContent
from superposition_sdk_python.models.update_default_config_response_content import UpdateDefaultConfigResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    key = 'key_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    update_default_config_request_content = superposition_sdk_python.UpdateDefaultConfigRequestContent() # UpdateDefaultConfigRequestContent | 

    try:
        api_response = api_instance.update_default_config(key, x_org_id, x_tenant, update_default_config_request_content)
        print("The response of DefaultApi->update_default_config:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->update_default_config: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **key** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **update_default_config_request_content** | [**UpdateDefaultConfigRequestContent**](UpdateDefaultConfigRequestContent.md)|  | 

### Return type

[**UpdateDefaultConfigResponseContent**](UpdateDefaultConfigResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | UpdateDefaultConfig 200 response |  -  |
**404** | ResourceNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_dimension**
> UpdateDimensionResponseContent update_dimension(dimension, x_org_id, x_tenant, update_dimension_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.update_dimension_request_content import UpdateDimensionRequestContent
from superposition_sdk_python.models.update_dimension_response_content import UpdateDimensionResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    dimension = 'dimension_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    update_dimension_request_content = superposition_sdk_python.UpdateDimensionRequestContent() # UpdateDimensionRequestContent | 

    try:
        api_response = api_instance.update_dimension(dimension, x_org_id, x_tenant, update_dimension_request_content)
        print("The response of DefaultApi->update_dimension:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->update_dimension: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **dimension** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **update_dimension_request_content** | [**UpdateDimensionRequestContent**](UpdateDimensionRequestContent.md)|  | 

### Return type

[**UpdateDimensionResponseContent**](UpdateDimensionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | UpdateDimension 200 response |  -  |
**404** | ResourceNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_function**
> UpdateFunctionResponseContent update_function(function_name, x_org_id, x_tenant, update_function_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.update_function_request_content import UpdateFunctionRequestContent
from superposition_sdk_python.models.update_function_response_content import UpdateFunctionResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    function_name = 'function_name_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    update_function_request_content = superposition_sdk_python.UpdateFunctionRequestContent() # UpdateFunctionRequestContent | 

    try:
        api_response = api_instance.update_function(function_name, x_org_id, x_tenant, update_function_request_content)
        print("The response of DefaultApi->update_function:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->update_function: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **function_name** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **update_function_request_content** | [**UpdateFunctionRequestContent**](UpdateFunctionRequestContent.md)|  | 

### Return type

[**UpdateFunctionResponseContent**](UpdateFunctionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | UpdateFunction 200 response |  -  |
**404** | FunctionNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_organisation**
> UpdateOrganisationResponseContent update_organisation(id, update_organisation_request_content=update_organisation_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.update_organisation_request_content import UpdateOrganisationRequestContent
from superposition_sdk_python.models.update_organisation_response_content import UpdateOrganisationResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    id = 'id_example' # str | 
    update_organisation_request_content = superposition_sdk_python.UpdateOrganisationRequestContent() # UpdateOrganisationRequestContent |  (optional)

    try:
        api_response = api_instance.update_organisation(id, update_organisation_request_content=update_organisation_request_content)
        print("The response of DefaultApi->update_organisation:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->update_organisation: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **str**|  | 
 **update_organisation_request_content** | [**UpdateOrganisationRequestContent**](UpdateOrganisationRequestContent.md)|  | [optional] 

### Return type

[**UpdateOrganisationResponseContent**](UpdateOrganisationResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | UpdateOrganisation 200 response |  -  |
**404** | OrganisationNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_override**
> UpdateOverrideResponseContent update_override(x_org_id, x_tenant, update_override_request_content, x_config_tags=x_config_tags)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.update_override_request_content import UpdateOverrideRequestContent
from superposition_sdk_python.models.update_override_response_content import UpdateOverrideResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    update_override_request_content = superposition_sdk_python.UpdateOverrideRequestContent() # UpdateOverrideRequestContent | 
    x_config_tags = 'x_config_tags_example' # str |  (optional)

    try:
        api_response = api_instance.update_override(x_org_id, x_tenant, update_override_request_content, x_config_tags=x_config_tags)
        print("The response of DefaultApi->update_override:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->update_override: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **update_override_request_content** | [**UpdateOverrideRequestContent**](UpdateOverrideRequestContent.md)|  | 
 **x_config_tags** | **str**|  | [optional] 

### Return type

[**UpdateOverrideResponseContent**](UpdateOverrideResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | UpdateOverride 200 response |  -  |
**404** | ResourceNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_overrides_experiment**
> UpdateOverridesExperimentResponseContent update_overrides_experiment(id, x_org_id, x_tenant, update_overrides_experiment_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.update_overrides_experiment_request_content import UpdateOverridesExperimentRequestContent
from superposition_sdk_python.models.update_overrides_experiment_response_content import UpdateOverridesExperimentResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    id = 'id_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    update_overrides_experiment_request_content = superposition_sdk_python.UpdateOverridesExperimentRequestContent() # UpdateOverridesExperimentRequestContent | 

    try:
        api_response = api_instance.update_overrides_experiment(id, x_org_id, x_tenant, update_overrides_experiment_request_content)
        print("The response of DefaultApi->update_overrides_experiment:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->update_overrides_experiment: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **update_overrides_experiment_request_content** | [**UpdateOverridesExperimentRequestContent**](UpdateOverridesExperimentRequestContent.md)|  | 

### Return type

[**UpdateOverridesExperimentResponseContent**](UpdateOverridesExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | UpdateOverridesExperiment 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_type_templates**
> UpdateTypeTemplatesResponseContent update_type_templates(type_name, x_org_id, x_tenant, update_type_templates_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.update_type_templates_request_content import UpdateTypeTemplatesRequestContent
from superposition_sdk_python.models.update_type_templates_response_content import UpdateTypeTemplatesResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    type_name = 'type_name_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    update_type_templates_request_content = superposition_sdk_python.UpdateTypeTemplatesRequestContent() # UpdateTypeTemplatesRequestContent | 

    try:
        api_response = api_instance.update_type_templates(type_name, x_org_id, x_tenant, update_type_templates_request_content)
        print("The response of DefaultApi->update_type_templates:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->update_type_templates: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **type_name** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **update_type_templates_request_content** | [**UpdateTypeTemplatesRequestContent**](UpdateTypeTemplatesRequestContent.md)|  | 

### Return type

[**UpdateTypeTemplatesResponseContent**](UpdateTypeTemplatesResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | UpdateTypeTemplates 200 response |  -  |
**404** | TypeTemplatesNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_workspace**
> UpdateWorkspaceResponseContent update_workspace(workspace_name, x_org_id, update_workspace_request_content)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.update_workspace_request_content import UpdateWorkspaceRequestContent
from superposition_sdk_python.models.update_workspace_response_content import UpdateWorkspaceResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    workspace_name = 'workspace_name_example' # str | 
    x_org_id = 'juspay' # str |  (default to 'juspay')
    update_workspace_request_content = superposition_sdk_python.UpdateWorkspaceRequestContent() # UpdateWorkspaceRequestContent | 

    try:
        api_response = api_instance.update_workspace(workspace_name, x_org_id, update_workspace_request_content)
        print("The response of DefaultApi->update_workspace:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->update_workspace: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **workspace_name** | **str**|  | 
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **update_workspace_request_content** | [**UpdateWorkspaceRequestContent**](UpdateWorkspaceRequestContent.md)|  | 

### Return type

[**UpdateWorkspaceResponseContent**](UpdateWorkspaceResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | UpdateWorkspace 200 response |  -  |
**404** | WorkspaceNotFound 404 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **weight_recompute**
> WeightRecomputeResponseContent weight_recompute(x_org_id, x_tenant, x_config_tags=x_config_tags)

### Example

* Bearer Authentication (smithy.api.httpBearerAuth):

```python
import superposition_sdk_python
from superposition_sdk_python.models.weight_recompute_response_content import WeightRecomputeResponseContent
from superposition_sdk_python.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = superposition_sdk_python.Configuration(
    host = "http://localhost"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: smithy.api.httpBearerAuth
configuration = superposition_sdk_python.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with superposition_sdk_python.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = superposition_sdk_python.DefaultApi(api_client)
    x_org_id = 'juspay' # str |  (default to 'juspay')
    x_tenant = 'x_tenant_example' # str | 
    x_config_tags = 'x_config_tags_example' # str |  (optional)

    try:
        api_response = api_instance.weight_recompute(x_org_id, x_tenant, x_config_tags=x_config_tags)
        print("The response of DefaultApi->weight_recompute:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->weight_recompute: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **x_org_id** | **str**|  | [default to &#39;juspay&#39;]
 **x_tenant** | **str**|  | 
 **x_config_tags** | **str**|  | [optional] 

### Return type

[**WeightRecomputeResponseContent**](WeightRecomputeResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | WeightRecompute 200 response |  -  |
**500** | InternalServerError 500 response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

