# \DefaultAPI

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**ConcludeExperiment**](DefaultAPI.md#ConcludeExperiment) | **Patch** /experiments/{id}/conclude | 
[**CreateContext**](DefaultAPI.md#CreateContext) | **Put** /context | 
[**CreateDefaultConfig**](DefaultAPI.md#CreateDefaultConfig) | **Post** /default-config | 
[**CreateDimension**](DefaultAPI.md#CreateDimension) | **Post** /dimension | 
[**CreateExperiment**](DefaultAPI.md#CreateExperiment) | **Post** /experiments | 
[**CreateFunction**](DefaultAPI.md#CreateFunction) | **Post** /function | 
[**CreateTypeTemplates**](DefaultAPI.md#CreateTypeTemplates) | **Post** /types | 
[**CreateWorkspace**](DefaultAPI.md#CreateWorkspace) | **Post** /workspaces | 
[**CreaterOrganisation**](DefaultAPI.md#CreaterOrganisation) | **Post** /superposition/organisations | 
[**DeleteContext**](DefaultAPI.md#DeleteContext) | **Delete** /context/{id} | 
[**DeleteDefaultConfig**](DefaultAPI.md#DeleteDefaultConfig) | **Delete** /default-config/{key} | 
[**DeleteDimension**](DefaultAPI.md#DeleteDimension) | **Delete** /dimension/{dimension} | 
[**DeleteFunction**](DefaultAPI.md#DeleteFunction) | **Delete** /function/{function_name} | 
[**DeleteTypeTemplates**](DefaultAPI.md#DeleteTypeTemplates) | **Delete** /types/{type_name} | 
[**DiscardExperiment**](DefaultAPI.md#DiscardExperiment) | **Patch** /experiments/{id}/discard | 
[**GetConfigFast**](DefaultAPI.md#GetConfigFast) | **Get** /config/fast | 
[**GetContext**](DefaultAPI.md#GetContext) | **Get** /context/{id} | 
[**GetContextFromCondition**](DefaultAPI.md#GetContextFromCondition) | **Post** /context/get | 
[**GetExperiment**](DefaultAPI.md#GetExperiment) | **Get** /experiments/{id} | 
[**GetFunction**](DefaultAPI.md#GetFunction) | **Get** /function/{function_name} | 
[**GetOrganisation**](DefaultAPI.md#GetOrganisation) | **Get** /superposition/organisations/{id} | 
[**GetTypeTemplatesList**](DefaultAPI.md#GetTypeTemplatesList) | **Get** /types | 
[**ListAuditLogs**](DefaultAPI.md#ListAuditLogs) | **Get** /audit | 
[**ListContexts**](DefaultAPI.md#ListContexts) | **Get** /context/list | 
[**ListDefaultConfigs**](DefaultAPI.md#ListDefaultConfigs) | **Get** /default-config | 
[**ListDimensions**](DefaultAPI.md#ListDimensions) | **Get** /dimension | 
[**ListExperiment**](DefaultAPI.md#ListExperiment) | **Get** /experiments | 
[**ListOrganisation**](DefaultAPI.md#ListOrganisation) | **Get** /superposition/organisations | 
[**ListVersions**](DefaultAPI.md#ListVersions) | **Get** /config/versions | 
[**ListWorkspace**](DefaultAPI.md#ListWorkspace) | **Get** /workspaces | 
[**MoveContext**](DefaultAPI.md#MoveContext) | **Put** /context/move/{id} | 
[**Publish**](DefaultAPI.md#Publish) | **Put** /function/{function_name}/publish | 
[**RampExperiment**](DefaultAPI.md#RampExperiment) | **Patch** /experiments/{id}/ramp | 
[**Test**](DefaultAPI.md#Test) | **Put** /function/{function_name}/{stage}/test | 
[**UpdateDefaultConfig**](DefaultAPI.md#UpdateDefaultConfig) | **Put** /default-config/{key} | 
[**UpdateDimension**](DefaultAPI.md#UpdateDimension) | **Put** /dimension/{dimension} | 
[**UpdateFunction**](DefaultAPI.md#UpdateFunction) | **Patch** /function/{function_name} | 
[**UpdateOrganisation**](DefaultAPI.md#UpdateOrganisation) | **Put** /superposition/organisations/{id} | 
[**UpdateOverride**](DefaultAPI.md#UpdateOverride) | **Put** /context/overrides | 
[**UpdateOverridesExperiment**](DefaultAPI.md#UpdateOverridesExperiment) | **Put** /experiments/{id}/overrides | 
[**UpdateTypeTemplates**](DefaultAPI.md#UpdateTypeTemplates) | **Put** /types/{type_name} | 
[**UpdateWorkspace**](DefaultAPI.md#UpdateWorkspace) | **Put** /workspaces/{workspace_name} | 
[**WeightRecompute**](DefaultAPI.md#WeightRecompute) | **Put** /weight/recompute | 



## ConcludeExperiment

> ConcludeExperimentResponseContent ConcludeExperiment(ctx, id).XOrgId(xOrgId).XTenant(xTenant).ConcludeExperimentRequestContent(concludeExperimentRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	id := "id_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	concludeExperimentRequestContent := *openapiclient.NewConcludeExperimentRequestContent("ChosenVariant_example", "ChangeReason_example") // ConcludeExperimentRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.ConcludeExperiment(context.Background(), id).XOrgId(xOrgId).XTenant(xTenant).ConcludeExperimentRequestContent(concludeExperimentRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.ConcludeExperiment``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `ConcludeExperiment`: ConcludeExperimentResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.ConcludeExperiment`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**id** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiConcludeExperimentRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **concludeExperimentRequestContent** | [**ConcludeExperimentRequestContent**](ConcludeExperimentRequestContent.md) |  | 

### Return type

[**ConcludeExperimentResponseContent**](ConcludeExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## CreateContext

> CreateContextResponseContent CreateContext(ctx).XOrgId(xOrgId).XTenant(xTenant).CreateContextRequestContent(createContextRequestContent).XConfigTags(xConfigTags).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	createContextRequestContent := *openapiclient.NewCreateContextRequestContent(map[string]interface{}{"key": interface{}(123)}, map[string]interface{}{"key": interface{}(123)}, "ChangeReason_example") // CreateContextRequestContent | 
	xConfigTags := "xConfigTags_example" // string |  (optional)

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.CreateContext(context.Background()).XOrgId(xOrgId).XTenant(xTenant).CreateContextRequestContent(createContextRequestContent).XConfigTags(xConfigTags).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.CreateContext``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `CreateContext`: CreateContextResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.CreateContext`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiCreateContextRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **createContextRequestContent** | [**CreateContextRequestContent**](CreateContextRequestContent.md) |  | 
 **xConfigTags** | **string** |  | 

### Return type

[**CreateContextResponseContent**](CreateContextResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## CreateDefaultConfig

> CreateDefaultConfigResponseContent CreateDefaultConfig(ctx).XOrgId(xOrgId).XTenant(xTenant).CreateDefaultConfigRequestContent(createDefaultConfigRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	createDefaultConfigRequestContent := *openapiclient.NewCreateDefaultConfigRequestContent("Key_example", interface{}(123), interface{}(123), "Description_example", "ChangeReason_example") // CreateDefaultConfigRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.CreateDefaultConfig(context.Background()).XOrgId(xOrgId).XTenant(xTenant).CreateDefaultConfigRequestContent(createDefaultConfigRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.CreateDefaultConfig``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `CreateDefaultConfig`: CreateDefaultConfigResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.CreateDefaultConfig`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiCreateDefaultConfigRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **createDefaultConfigRequestContent** | [**CreateDefaultConfigRequestContent**](CreateDefaultConfigRequestContent.md) |  | 

### Return type

[**CreateDefaultConfigResponseContent**](CreateDefaultConfigResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## CreateDimension

> CreateDimensionResponseContent CreateDimension(ctx).XOrgId(xOrgId).XTenant(xTenant).CreateDimensionRequestContent(createDimensionRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	createDimensionRequestContent := *openapiclient.NewCreateDimensionRequestContent("Dimension_example", float32(123), interface{}(123), "Description_example", "ChangeReason_example") // CreateDimensionRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.CreateDimension(context.Background()).XOrgId(xOrgId).XTenant(xTenant).CreateDimensionRequestContent(createDimensionRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.CreateDimension``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `CreateDimension`: CreateDimensionResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.CreateDimension`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiCreateDimensionRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **createDimensionRequestContent** | [**CreateDimensionRequestContent**](CreateDimensionRequestContent.md) |  | 

### Return type

[**CreateDimensionResponseContent**](CreateDimensionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## CreateExperiment

> CreateExperimentResponseContent CreateExperiment(ctx).XOrgId(xOrgId).XTenant(xTenant).CreateExperimentRequestContent(createExperimentRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	createExperimentRequestContent := *openapiclient.NewCreateExperimentRequestContent("Name_example", map[string]interface{}{"key": interface{}(123)}, []openapiclient.Variant{*openapiclient.NewVariant("Id_example", "VariantType_example", []interface{}{nil})}, "Description_example", "ChangeReason_example") // CreateExperimentRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.CreateExperiment(context.Background()).XOrgId(xOrgId).XTenant(xTenant).CreateExperimentRequestContent(createExperimentRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.CreateExperiment``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `CreateExperiment`: CreateExperimentResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.CreateExperiment`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiCreateExperimentRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **createExperimentRequestContent** | [**CreateExperimentRequestContent**](CreateExperimentRequestContent.md) |  | 

### Return type

[**CreateExperimentResponseContent**](CreateExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## CreateFunction

> CreateFunctionResponseContent CreateFunction(ctx).XOrgId(xOrgId).XTenant(xTenant).CreateFunctionRequestContent(createFunctionRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	createFunctionRequestContent := *openapiclient.NewCreateFunctionRequestContent("FunctionName_example", "Description_example", "ChangeReason_example", "Function_example", "RuntimeVersion_example") // CreateFunctionRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.CreateFunction(context.Background()).XOrgId(xOrgId).XTenant(xTenant).CreateFunctionRequestContent(createFunctionRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.CreateFunction``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `CreateFunction`: CreateFunctionResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.CreateFunction`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiCreateFunctionRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **createFunctionRequestContent** | [**CreateFunctionRequestContent**](CreateFunctionRequestContent.md) |  | 

### Return type

[**CreateFunctionResponseContent**](CreateFunctionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## CreateTypeTemplates

> CreateTypeTemplatesResponseContent CreateTypeTemplates(ctx).XOrgId(xOrgId).XTenant(xTenant).CreateTypeTemplatesRequestContent(createTypeTemplatesRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	createTypeTemplatesRequestContent := *openapiclient.NewCreateTypeTemplatesRequestContent("TypeName_example", interface{}(123), "Description_example", "ChangeReason_example") // CreateTypeTemplatesRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.CreateTypeTemplates(context.Background()).XOrgId(xOrgId).XTenant(xTenant).CreateTypeTemplatesRequestContent(createTypeTemplatesRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.CreateTypeTemplates``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `CreateTypeTemplates`: CreateTypeTemplatesResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.CreateTypeTemplates`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiCreateTypeTemplatesRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **createTypeTemplatesRequestContent** | [**CreateTypeTemplatesRequestContent**](CreateTypeTemplatesRequestContent.md) |  | 

### Return type

[**CreateTypeTemplatesResponseContent**](CreateTypeTemplatesResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## CreateWorkspace

> CreateWorkspaceResponseContent CreateWorkspace(ctx).XOrgId(xOrgId).CreateWorkspaceRequestContent(createWorkspaceRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	createWorkspaceRequestContent := *openapiclient.NewCreateWorkspaceRequestContent("WorkspaceAdminEmail_example", "WorkspaceName_example") // CreateWorkspaceRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.CreateWorkspace(context.Background()).XOrgId(xOrgId).CreateWorkspaceRequestContent(createWorkspaceRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.CreateWorkspace``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `CreateWorkspace`: CreateWorkspaceResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.CreateWorkspace`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiCreateWorkspaceRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **createWorkspaceRequestContent** | [**CreateWorkspaceRequestContent**](CreateWorkspaceRequestContent.md) |  | 

### Return type

[**CreateWorkspaceResponseContent**](CreateWorkspaceResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## CreaterOrganisation

> CreaterOrganisationResponseContent CreaterOrganisation(ctx).CreaterOrganisationRequestContent(createrOrganisationRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	createrOrganisationRequestContent := *openapiclient.NewCreaterOrganisationRequestContent("AdminEmail_example", "Name_example") // CreaterOrganisationRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.CreaterOrganisation(context.Background()).CreaterOrganisationRequestContent(createrOrganisationRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.CreaterOrganisation``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `CreaterOrganisation`: CreaterOrganisationResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.CreaterOrganisation`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiCreaterOrganisationRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **createrOrganisationRequestContent** | [**CreaterOrganisationRequestContent**](CreaterOrganisationRequestContent.md) |  | 

### Return type

[**CreaterOrganisationResponseContent**](CreaterOrganisationResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## DeleteContext

> DeleteContext(ctx, id).XOrgId(xOrgId).XTenant(xTenant).XConfigTags(xConfigTags).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	id := "id_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	xConfigTags := "xConfigTags_example" // string |  (optional)

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	r, err := apiClient.DefaultAPI.DeleteContext(context.Background(), id).XOrgId(xOrgId).XTenant(xTenant).XConfigTags(xConfigTags).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.DeleteContext``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**id** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiDeleteContextRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **xConfigTags** | **string** |  | 

### Return type

 (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## DeleteDefaultConfig

> DeleteDefaultConfig(ctx, key).XOrgId(xOrgId).XTenant(xTenant).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	key := "key_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	r, err := apiClient.DefaultAPI.DeleteDefaultConfig(context.Background(), key).XOrgId(xOrgId).XTenant(xTenant).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.DeleteDefaultConfig``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**key** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiDeleteDefaultConfigRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 

### Return type

 (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## DeleteDimension

> DeleteDimension(ctx, dimension).XOrgId(xOrgId).XTenant(xTenant).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	dimension := "dimension_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	r, err := apiClient.DefaultAPI.DeleteDimension(context.Background(), dimension).XOrgId(xOrgId).XTenant(xTenant).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.DeleteDimension``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**dimension** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiDeleteDimensionRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 

### Return type

 (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## DeleteFunction

> DeleteFunction(ctx, functionName).XOrgId(xOrgId).XTenant(xTenant).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	functionName := "functionName_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	r, err := apiClient.DefaultAPI.DeleteFunction(context.Background(), functionName).XOrgId(xOrgId).XTenant(xTenant).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.DeleteFunction``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**functionName** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiDeleteFunctionRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 

### Return type

 (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## DeleteTypeTemplates

> DeleteTypeTemplates(ctx, typeName).XOrgId(xOrgId).XTenant(xTenant).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	typeName := "typeName_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	r, err := apiClient.DefaultAPI.DeleteTypeTemplates(context.Background(), typeName).XOrgId(xOrgId).XTenant(xTenant).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.DeleteTypeTemplates``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**typeName** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiDeleteTypeTemplatesRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 

### Return type

 (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## DiscardExperiment

> DiscardExperimentResponseContent DiscardExperiment(ctx, id).XOrgId(xOrgId).XTenant(xTenant).DiscardExperimentRequestContent(discardExperimentRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	id := "id_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	discardExperimentRequestContent := *openapiclient.NewDiscardExperimentRequestContent("ChangeReason_example") // DiscardExperimentRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.DiscardExperiment(context.Background(), id).XOrgId(xOrgId).XTenant(xTenant).DiscardExperimentRequestContent(discardExperimentRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.DiscardExperiment``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `DiscardExperiment`: DiscardExperimentResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.DiscardExperiment`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**id** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiDiscardExperimentRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **discardExperimentRequestContent** | [**DiscardExperimentRequestContent**](DiscardExperimentRequestContent.md) |  | 

### Return type

[**DiscardExperimentResponseContent**](DiscardExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## GetConfigFast

> interface{} GetConfigFast(ctx).XOrgId(xOrgId).XTenant(xTenant).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.GetConfigFast(context.Background()).XOrgId(xOrgId).XTenant(xTenant).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.GetConfigFast``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `GetConfigFast`: interface{}
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.GetConfigFast`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiGetConfigFastRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 

### Return type

**interface{}**

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## GetContext

> GetContextResponseContent GetContext(ctx, id).XOrgId(xOrgId).XTenant(xTenant).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	id := "id_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.GetContext(context.Background(), id).XOrgId(xOrgId).XTenant(xTenant).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.GetContext``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `GetContext`: GetContextResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.GetContext`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**id** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiGetContextRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 

### Return type

[**GetContextResponseContent**](GetContextResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## GetContextFromCondition

> GetContextFromConditionResponseContent GetContextFromCondition(ctx).XOrgId(xOrgId).XTenant(xTenant).Body(body).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	body := interface{}(987) // interface{} |  (optional)

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.GetContextFromCondition(context.Background()).XOrgId(xOrgId).XTenant(xTenant).Body(body).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.GetContextFromCondition``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `GetContextFromCondition`: GetContextFromConditionResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.GetContextFromCondition`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiGetContextFromConditionRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **body** | **interface{}** |  | 

### Return type

[**GetContextFromConditionResponseContent**](GetContextFromConditionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## GetExperiment

> GetExperimentResponseContent GetExperiment(ctx, id).XOrgId(xOrgId).XTenant(xTenant).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	id := "id_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.GetExperiment(context.Background(), id).XOrgId(xOrgId).XTenant(xTenant).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.GetExperiment``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `GetExperiment`: GetExperimentResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.GetExperiment`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**id** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiGetExperimentRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 

### Return type

[**GetExperimentResponseContent**](GetExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## GetFunction

> GetFunctionResponseContent GetFunction(ctx, functionName).XOrgId(xOrgId).XTenant(xTenant).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	functionName := "functionName_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.GetFunction(context.Background(), functionName).XOrgId(xOrgId).XTenant(xTenant).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.GetFunction``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `GetFunction`: GetFunctionResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.GetFunction`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**functionName** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiGetFunctionRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 

### Return type

[**GetFunctionResponseContent**](GetFunctionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## GetOrganisation

> GetOrganisationResponseContent GetOrganisation(ctx, id).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	id := "id_example" // string | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.GetOrganisation(context.Background(), id).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.GetOrganisation``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `GetOrganisation`: GetOrganisationResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.GetOrganisation`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**id** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiGetOrganisationRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------


### Return type

[**GetOrganisationResponseContent**](GetOrganisationResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## GetTypeTemplatesList

> GetTypeTemplatesListResponseContent GetTypeTemplatesList(ctx).XOrgId(xOrgId).XTenant(xTenant).Page(page).Count(count).All(all).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	page := float32(8.14) // float32 |  (optional)
	count := float32(8.14) // float32 |  (optional)
	all := true // bool |  (optional)

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.GetTypeTemplatesList(context.Background()).XOrgId(xOrgId).XTenant(xTenant).Page(page).Count(count).All(all).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.GetTypeTemplatesList``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `GetTypeTemplatesList`: GetTypeTemplatesListResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.GetTypeTemplatesList`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiGetTypeTemplatesListRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **page** | **float32** |  | 
 **count** | **float32** |  | 
 **all** | **bool** |  | 

### Return type

[**GetTypeTemplatesListResponseContent**](GetTypeTemplatesListResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## ListAuditLogs

> ListAuditLogsResponseContent ListAuditLogs(ctx).XOrgId(xOrgId).XTenant(xTenant).Count(count).Page(page).FromDate(fromDate).ToDate(toDate).Table(table).Action(action).Username(username).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
    "time"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	count := float32(8.14) // float32 |  (optional)
	page := float32(8.14) // float32 |  (optional)
	fromDate := time.Now() // time.Time |  (optional)
	toDate := time.Now() // time.Time |  (optional)
	table := "table_example" // string | Comma serparated list of tables. (optional)
	action := "action_example" // string | Comma serparated list of actions. (optional)
	username := "username_example" // string |  (optional)

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.ListAuditLogs(context.Background()).XOrgId(xOrgId).XTenant(xTenant).Count(count).Page(page).FromDate(fromDate).ToDate(toDate).Table(table).Action(action).Username(username).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.ListAuditLogs``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `ListAuditLogs`: ListAuditLogsResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.ListAuditLogs`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiListAuditLogsRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **count** | **float32** |  | 
 **page** | **float32** |  | 
 **fromDate** | **time.Time** |  | 
 **toDate** | **time.Time** |  | 
 **table** | **string** | Comma serparated list of tables. | 
 **action** | **string** | Comma serparated list of actions. | 
 **username** | **string** |  | 

### Return type

[**ListAuditLogsResponseContent**](ListAuditLogsResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## ListContexts

> ListContextsResponseContent ListContexts(ctx).XOrgId(xOrgId).XTenant(xTenant).Page(page).Size(size).Prefix(prefix).SortOn(sortOn).SortBy(sortBy).CreatedBy(createdBy).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	page := float32(8.14) // float32 |  (optional)
	size := float32(8.14) // float32 |  (optional)
	prefix := "prefix_example" // string |  (optional)
	sortOn := openapiclient.ContextFilterSortOn("CreatedAt") // ContextFilterSortOn |  (optional)
	sortBy := openapiclient.SortBy("Desc") // SortBy |  (optional)
	createdBy := "createdBy_example" // string |  (optional)

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.ListContexts(context.Background()).XOrgId(xOrgId).XTenant(xTenant).Page(page).Size(size).Prefix(prefix).SortOn(sortOn).SortBy(sortBy).CreatedBy(createdBy).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.ListContexts``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `ListContexts`: ListContextsResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.ListContexts`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiListContextsRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **page** | **float32** |  | 
 **size** | **float32** |  | 
 **prefix** | **string** |  | 
 **sortOn** | [**ContextFilterSortOn**](ContextFilterSortOn.md) |  | 
 **sortBy** | [**SortBy**](SortBy.md) |  | 
 **createdBy** | **string** |  | 

### Return type

[**ListContextsResponseContent**](ListContextsResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## ListDefaultConfigs

> ListDefaultConfigsResponseContent ListDefaultConfigs(ctx).XOrgId(xOrgId).XTenant(xTenant).Count(count).Page(page).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	count := float32(8.14) // float32 |  (optional)
	page := float32(8.14) // float32 |  (optional)

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.ListDefaultConfigs(context.Background()).XOrgId(xOrgId).XTenant(xTenant).Count(count).Page(page).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.ListDefaultConfigs``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `ListDefaultConfigs`: ListDefaultConfigsResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.ListDefaultConfigs`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiListDefaultConfigsRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **count** | **float32** |  | 
 **page** | **float32** |  | 

### Return type

[**ListDefaultConfigsResponseContent**](ListDefaultConfigsResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## ListDimensions

> ListDimensionsResponseContent ListDimensions(ctx).XOrgId(xOrgId).XTenant(xTenant).Count(count).Page(page).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	count := float32(8.14) // float32 |  (optional)
	page := float32(8.14) // float32 |  (optional)

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.ListDimensions(context.Background()).XOrgId(xOrgId).XTenant(xTenant).Count(count).Page(page).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.ListDimensions``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `ListDimensions`: ListDimensionsResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.ListDimensions`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiListDimensionsRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **count** | **float32** |  | 
 **page** | **float32** |  | 

### Return type

[**ListDimensionsResponseContent**](ListDimensionsResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## ListExperiment

> ListExperimentResponseContent ListExperiment(ctx).XOrgId(xOrgId).XTenant(xTenant).Page(page).Count(count).All(all).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	page := float32(8.14) // float32 |  (optional)
	count := float32(8.14) // float32 |  (optional)
	all := true // bool |  (optional)

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.ListExperiment(context.Background()).XOrgId(xOrgId).XTenant(xTenant).Page(page).Count(count).All(all).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.ListExperiment``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `ListExperiment`: ListExperimentResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.ListExperiment`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiListExperimentRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **page** | **float32** |  | 
 **count** | **float32** |  | 
 **all** | **bool** |  | 

### Return type

[**ListExperimentResponseContent**](ListExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## ListOrganisation

> ListOrganisationResponseContent ListOrganisation(ctx).Page(page).Count(count).All(all).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	page := float32(8.14) // float32 |  (optional)
	count := float32(8.14) // float32 |  (optional)
	all := true // bool |  (optional)

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.ListOrganisation(context.Background()).Page(page).Count(count).All(all).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.ListOrganisation``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `ListOrganisation`: ListOrganisationResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.ListOrganisation`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiListOrganisationRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **page** | **float32** |  | 
 **count** | **float32** |  | 
 **all** | **bool** |  | 

### Return type

[**ListOrganisationResponseContent**](ListOrganisationResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## ListVersions

> ListVersionsResponseContent ListVersions(ctx).XOrgId(xOrgId).XTenant(xTenant).Count(count).Page(page).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	count := float32(8.14) // float32 |  (optional)
	page := float32(8.14) // float32 |  (optional)

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.ListVersions(context.Background()).XOrgId(xOrgId).XTenant(xTenant).Count(count).Page(page).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.ListVersions``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `ListVersions`: ListVersionsResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.ListVersions`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiListVersionsRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **count** | **float32** |  | 
 **page** | **float32** |  | 

### Return type

[**ListVersionsResponseContent**](ListVersionsResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## ListWorkspace

> ListWorkspaceResponseContent ListWorkspace(ctx).XOrgId(xOrgId).Page(page).Count(count).All(all).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	page := float32(8.14) // float32 |  (optional)
	count := float32(8.14) // float32 |  (optional)
	all := true // bool |  (optional)

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.ListWorkspace(context.Background()).XOrgId(xOrgId).Page(page).Count(count).All(all).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.ListWorkspace``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `ListWorkspace`: ListWorkspaceResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.ListWorkspace`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiListWorkspaceRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **page** | **float32** |  | 
 **count** | **float32** |  | 
 **all** | **bool** |  | 

### Return type

[**ListWorkspaceResponseContent**](ListWorkspaceResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## MoveContext

> MoveContextResponseContent MoveContext(ctx, id).XOrgId(xOrgId).XTenant(xTenant).MoveContextRequestContent(moveContextRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	id := "id_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	moveContextRequestContent := *openapiclient.NewMoveContextRequestContent(map[string]interface{}{"key": interface{}(123)}, "ChangeReason_example") // MoveContextRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.MoveContext(context.Background(), id).XOrgId(xOrgId).XTenant(xTenant).MoveContextRequestContent(moveContextRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.MoveContext``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `MoveContext`: MoveContextResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.MoveContext`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**id** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiMoveContextRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **moveContextRequestContent** | [**MoveContextRequestContent**](MoveContextRequestContent.md) |  | 

### Return type

[**MoveContextResponseContent**](MoveContextResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## Publish

> PublishResponseContent Publish(ctx, functionName).XOrgId(xOrgId).XTenant(xTenant).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	functionName := "functionName_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.Publish(context.Background(), functionName).XOrgId(xOrgId).XTenant(xTenant).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.Publish``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `Publish`: PublishResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.Publish`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**functionName** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiPublishRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 

### Return type

[**PublishResponseContent**](PublishResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## RampExperiment

> RampExperimentResponseContent RampExperiment(ctx, id).XOrgId(xOrgId).XTenant(xTenant).RampExperimentRequestContent(rampExperimentRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	id := "id_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	rampExperimentRequestContent := *openapiclient.NewRampExperimentRequestContent("ChangeReason_example", float32(123)) // RampExperimentRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.RampExperiment(context.Background(), id).XOrgId(xOrgId).XTenant(xTenant).RampExperimentRequestContent(rampExperimentRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.RampExperiment``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `RampExperiment`: RampExperimentResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.RampExperiment`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**id** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiRampExperimentRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **rampExperimentRequestContent** | [**RampExperimentRequestContent**](RampExperimentRequestContent.md) |  | 

### Return type

[**RampExperimentResponseContent**](RampExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## Test

> Test(ctx, functionName, stage).XOrgId(xOrgId).XTenant(xTenant).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	functionName := "functionName_example" // string | 
	stage := openapiclient.Stage("draft") // Stage | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	r, err := apiClient.DefaultAPI.Test(context.Background(), functionName, stage).XOrgId(xOrgId).XTenant(xTenant).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.Test``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**functionName** | **string** |  | 
**stage** | [**Stage**](.md) |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiTestRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------


 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 

### Return type

 (empty response body)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## UpdateDefaultConfig

> UpdateDefaultConfigResponseContent UpdateDefaultConfig(ctx, key).XOrgId(xOrgId).XTenant(xTenant).UpdateDefaultConfigRequestContent(updateDefaultConfigRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	key := "key_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	updateDefaultConfigRequestContent := *openapiclient.NewUpdateDefaultConfigRequestContent("ChangeReason_example") // UpdateDefaultConfigRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.UpdateDefaultConfig(context.Background(), key).XOrgId(xOrgId).XTenant(xTenant).UpdateDefaultConfigRequestContent(updateDefaultConfigRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.UpdateDefaultConfig``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `UpdateDefaultConfig`: UpdateDefaultConfigResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.UpdateDefaultConfig`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**key** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiUpdateDefaultConfigRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **updateDefaultConfigRequestContent** | [**UpdateDefaultConfigRequestContent**](UpdateDefaultConfigRequestContent.md) |  | 

### Return type

[**UpdateDefaultConfigResponseContent**](UpdateDefaultConfigResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## UpdateDimension

> UpdateDimensionResponseContent UpdateDimension(ctx, dimension).XOrgId(xOrgId).XTenant(xTenant).UpdateDimensionRequestContent(updateDimensionRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	dimension := "dimension_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	updateDimensionRequestContent := *openapiclient.NewUpdateDimensionRequestContent("ChangeReason_example") // UpdateDimensionRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.UpdateDimension(context.Background(), dimension).XOrgId(xOrgId).XTenant(xTenant).UpdateDimensionRequestContent(updateDimensionRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.UpdateDimension``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `UpdateDimension`: UpdateDimensionResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.UpdateDimension`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**dimension** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiUpdateDimensionRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **updateDimensionRequestContent** | [**UpdateDimensionRequestContent**](UpdateDimensionRequestContent.md) |  | 

### Return type

[**UpdateDimensionResponseContent**](UpdateDimensionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## UpdateFunction

> UpdateFunctionResponseContent UpdateFunction(ctx, functionName).XOrgId(xOrgId).XTenant(xTenant).UpdateFunctionRequestContent(updateFunctionRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	functionName := "functionName_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	updateFunctionRequestContent := *openapiclient.NewUpdateFunctionRequestContent("ChangeReason_example", "Function_example", "RuntimeVersion_example") // UpdateFunctionRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.UpdateFunction(context.Background(), functionName).XOrgId(xOrgId).XTenant(xTenant).UpdateFunctionRequestContent(updateFunctionRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.UpdateFunction``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `UpdateFunction`: UpdateFunctionResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.UpdateFunction`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**functionName** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiUpdateFunctionRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **updateFunctionRequestContent** | [**UpdateFunctionRequestContent**](UpdateFunctionRequestContent.md) |  | 

### Return type

[**UpdateFunctionResponseContent**](UpdateFunctionResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## UpdateOrganisation

> UpdateOrganisationResponseContent UpdateOrganisation(ctx, id).UpdateOrganisationRequestContent(updateOrganisationRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	id := "id_example" // string | 
	updateOrganisationRequestContent := *openapiclient.NewUpdateOrganisationRequestContent() // UpdateOrganisationRequestContent |  (optional)

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.UpdateOrganisation(context.Background(), id).UpdateOrganisationRequestContent(updateOrganisationRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.UpdateOrganisation``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `UpdateOrganisation`: UpdateOrganisationResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.UpdateOrganisation`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**id** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiUpdateOrganisationRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **updateOrganisationRequestContent** | [**UpdateOrganisationRequestContent**](UpdateOrganisationRequestContent.md) |  | 

### Return type

[**UpdateOrganisationResponseContent**](UpdateOrganisationResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## UpdateOverride

> UpdateOverrideResponseContent UpdateOverride(ctx).XOrgId(xOrgId).XTenant(xTenant).UpdateOverrideRequestContent(updateOverrideRequestContent).XConfigTags(xConfigTags).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	updateOverrideRequestContent := *openapiclient.NewUpdateOverrideRequestContent(map[string]interface{}{"key": interface{}(123)}, map[string]interface{}{"key": interface{}(123)}, "ChangeReason_example") // UpdateOverrideRequestContent | 
	xConfigTags := "xConfigTags_example" // string |  (optional)

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.UpdateOverride(context.Background()).XOrgId(xOrgId).XTenant(xTenant).UpdateOverrideRequestContent(updateOverrideRequestContent).XConfigTags(xConfigTags).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.UpdateOverride``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `UpdateOverride`: UpdateOverrideResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.UpdateOverride`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiUpdateOverrideRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **updateOverrideRequestContent** | [**UpdateOverrideRequestContent**](UpdateOverrideRequestContent.md) |  | 
 **xConfigTags** | **string** |  | 

### Return type

[**UpdateOverrideResponseContent**](UpdateOverrideResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## UpdateOverridesExperiment

> UpdateOverridesExperimentResponseContent UpdateOverridesExperiment(ctx, id).XOrgId(xOrgId).XTenant(xTenant).UpdateOverridesExperimentRequestContent(updateOverridesExperimentRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	id := "id_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	updateOverridesExperimentRequestContent := *openapiclient.NewUpdateOverridesExperimentRequestContent([]openapiclient.Variant{*openapiclient.NewVariant("Id_example", "VariantType_example", []interface{}{nil})}, "ChangeReason_example") // UpdateOverridesExperimentRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.UpdateOverridesExperiment(context.Background(), id).XOrgId(xOrgId).XTenant(xTenant).UpdateOverridesExperimentRequestContent(updateOverridesExperimentRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.UpdateOverridesExperiment``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `UpdateOverridesExperiment`: UpdateOverridesExperimentResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.UpdateOverridesExperiment`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**id** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiUpdateOverridesExperimentRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **updateOverridesExperimentRequestContent** | [**UpdateOverridesExperimentRequestContent**](UpdateOverridesExperimentRequestContent.md) |  | 

### Return type

[**UpdateOverridesExperimentResponseContent**](UpdateOverridesExperimentResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## UpdateTypeTemplates

> UpdateTypeTemplatesResponseContent UpdateTypeTemplates(ctx, typeName).XOrgId(xOrgId).XTenant(xTenant).UpdateTypeTemplatesRequestContent(updateTypeTemplatesRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	typeName := "typeName_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	updateTypeTemplatesRequestContent := *openapiclient.NewUpdateTypeTemplatesRequestContent(interface{}(123), "ChangeReason_example") // UpdateTypeTemplatesRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.UpdateTypeTemplates(context.Background(), typeName).XOrgId(xOrgId).XTenant(xTenant).UpdateTypeTemplatesRequestContent(updateTypeTemplatesRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.UpdateTypeTemplates``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `UpdateTypeTemplates`: UpdateTypeTemplatesResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.UpdateTypeTemplates`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**typeName** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiUpdateTypeTemplatesRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **updateTypeTemplatesRequestContent** | [**UpdateTypeTemplatesRequestContent**](UpdateTypeTemplatesRequestContent.md) |  | 

### Return type

[**UpdateTypeTemplatesResponseContent**](UpdateTypeTemplatesResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## UpdateWorkspace

> UpdateWorkspaceResponseContent UpdateWorkspace(ctx, workspaceName).XOrgId(xOrgId).UpdateWorkspaceRequestContent(updateWorkspaceRequestContent).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	workspaceName := "workspaceName_example" // string | 
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	updateWorkspaceRequestContent := *openapiclient.NewUpdateWorkspaceRequestContent("WorkspaceAdminEmail_example") // UpdateWorkspaceRequestContent | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.UpdateWorkspace(context.Background(), workspaceName).XOrgId(xOrgId).UpdateWorkspaceRequestContent(updateWorkspaceRequestContent).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.UpdateWorkspace``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `UpdateWorkspace`: UpdateWorkspaceResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.UpdateWorkspace`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**workspaceName** | **string** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiUpdateWorkspaceRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **updateWorkspaceRequestContent** | [**UpdateWorkspaceRequestContent**](UpdateWorkspaceRequestContent.md) |  | 

### Return type

[**UpdateWorkspaceResponseContent**](UpdateWorkspaceResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## WeightRecompute

> WeightRecomputeResponseContent WeightRecompute(ctx).XOrgId(xOrgId).XTenant(xTenant).XConfigTags(xConfigTags).Execute()



### Example

```go
package main

import (
	"context"
	"fmt"
	"os"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
	xOrgId := "xOrgId_example" // string |  (default to "juspay")
	xTenant := "xTenant_example" // string | 
	xConfigTags := "xConfigTags_example" // string |  (optional)

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.WeightRecompute(context.Background()).XOrgId(xOrgId).XTenant(xTenant).XConfigTags(xConfigTags).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.WeightRecompute``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `WeightRecompute`: WeightRecomputeResponseContent
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.WeightRecompute`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiWeightRecomputeRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xOrgId** | **string** |  | [default to &quot;juspay&quot;]
 **xTenant** | **string** |  | 
 **xConfigTags** | **string** |  | 

### Return type

[**WeightRecomputeResponseContent**](WeightRecomputeResponseContent.md)

### Authorization

[smithy.api.httpBearerAuth](../README.md#smithy.api.httpBearerAuth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)

