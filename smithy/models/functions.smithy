// Smithy version, not API version.
$version: "2.0"

namespace io.superposition

resource Function {
    identifiers: {
        function_name: String
        workspace_id: String
        org_id: String
    }
    properties: {
        published_code: String
        draft_code: String
        description: String
        published_runtime_version: FunctionRuntimeVersion
        draft_runtime_version: FunctionRuntimeVersion
        published_at: DateTime
        draft_edited_at: DateTime
        published_by: String
        draft_edited_by: String
        last_modified_at: DateTime
        last_modified_by: String
        change_reason: String
        function_type: FunctionTypes
    }
    read: GetFunction
    update: UpdateFunction
    delete: DeleteFunction
    list: ListFunction
    operations: [
        CreateFunction
        Test
        Publish
    ]
}

enum FunctionRuntimeVersion {
    V1 = "1.0"
}

structure FunctionExecutionResponse {
    @notProperty
    @required
    fn_output: Document

    @notProperty
    @required
    stdout: String

    @notProperty
    @required
    function_type: FunctionTypes
}

union FunctionExecutionRequest for Function {
    value_validate: ValueValidationFunctionRequest
    value_compute: ValueComputeFunctionRequest
    context_validate: ContextValidationFunctionRequest
    change_reason_validate: ChangeReasonValidationFunctionRequest
}

structure ValueValidationFunctionRequest {
    @required
    key: String

    @required
    value: Document

    @required
    type: String

    @required
    environment: Document
}

structure ValueComputeFunctionRequest {
    @required
    name: String

    @required
    prefix: String

    @required
    type: String

    @required
    environment: Document
}

structure ContextValidationFunctionRequest {
    @required
    environment: Document
}

structure ChangeReasonValidationFunctionRequest {
    @required
    change_reason: String
}

enum FunctionTypes {
    VALUE_VALIDATION
    VALUE_COMPUTE
    CONTEXT_VALIDATION
    CHANGE_REASON_VALIDATION
}

list FunctionTypesList {
    member: FunctionTypes
}

enum Stage {
    DRAFT = "draft"
    PUBLISHED = "published"
}

structure CreateFunctionRequest for Function with [WorkspaceMixin] {
    @required
    $function_name

    @required
    $description

    @required
    $change_reason

    @required
    @notProperty
    function: String

    @required
    @notProperty
    runtime_version: FunctionRuntimeVersion

    @required
    $function_type
}

structure UpdateFunctionRequest for Function with [WorkspaceMixin] {
    @httpLabel
    @required
    $function_name

    $description

    @required
    $change_reason

    @notProperty
    function: String

    @notProperty
    runtime_version: FunctionRuntimeVersion
}

structure FunctionResponse for Function {
    @required
    $function_name

    $published_code

    @required
    $draft_code

    $published_runtime_version

    @required
    $draft_runtime_version

    $published_at

    @required
    $draft_edited_at

    $published_by

    @required
    $draft_edited_by

    @required
    $last_modified_at

    @required
    $last_modified_by

    @required
    $change_reason

    @required
    $description

    @required
    $function_type
}

list FunctionListResponse {
    member: FunctionResponse
}

// Operations
@documentation("Creates a new custom function for value_validation, value_compute, context_validation or change_reason_validation with specified code, runtime version, and function type.")
@http(method: "POST", uri: "/function")
@tags(["Functions"])
operation CreateFunction {
    input: CreateFunctionRequest
    output: FunctionResponse
}

@documentation("Retrieves detailed information about a specific function including its published and draft versions, code, and metadata.")
@readonly
@http(method: "GET", uri: "/function/{function_name}")
@tags(["Functions"])
operation GetFunction with [GetOperation] {
    input := for Function with [WorkspaceMixin] {
        @httpLabel
        @required
        $function_name
    }

    output: FunctionResponse
}

@documentation("Retrieves a paginated list of all functions in the workspace with their basic information and current status.")
@readonly
@http(method: "GET", uri: "/function")
@tags(["Functions"])
operation ListFunction {
    input := with [PaginationParams, WorkspaceMixin] {
        @httpQuery("function_type")
        function_type: FunctionTypesList
    }

    output := with [PaginatedResponse] {
        @required
        data: FunctionListResponse
    }
}

@documentation("Updates the draft version of an existing function with new code, runtime version, or description while preserving the published version.")
@idempotent
@http(method: "PATCH", uri: "/function/{function_name}")
@tags(["Functions"])
operation UpdateFunction with [GetOperation] {
    input: UpdateFunctionRequest
    output: FunctionResponse
}

@documentation("Permanently removes a function from the workspace, deleting both draft and published versions along with all associated code. It fails if already in use")
@idempotent
@http(method: "DELETE", uri: "/function/{function_name}", code: 204)
@tags(["Functions"])
operation DeleteFunction with [GetOperation] {
    input := for Function with [WorkspaceMixin] {
        @httpLabel
        @required
        $function_name
    }
}

@documentation("Executes a function in test mode with provided input parameters to validate its behavior before publishing or deployment.")
@idempotent
@http(method: "POST", uri: "/function/{function_name}/{stage}/test")
@tags(["Functions"])
operation Test with [GetOperation] {
    input := for Function with [WorkspaceMixin] {
        @httpLabel
        @required
        $function_name

        @httpLabel
        @required
        @notProperty
        stage: Stage

        @httpPayload
        @required
        @notProperty
        request: FunctionExecutionRequest
    }

    output: FunctionExecutionResponse
}

@documentation("Publishes the draft version of a function, making it the active version used for value_validation, value_compute, context_validation or change_reason_validation in the system.")
@idempotent
@http(method: "PATCH", uri: "/function/{function_name}/publish")
@tags(["Functions"])
operation Publish with [GetOperation] {
    input := for Function with [WorkspaceMixin] {
        @httpLabel
        @required
        $function_name

        @required
        $change_reason
    }

    output: FunctionResponse
}
