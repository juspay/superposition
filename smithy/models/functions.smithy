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
        published_runtime_version: String
        draft_runtime_version: String
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
    put: UpdateFunction
    delete: DeleteFunction
    list: ListFunction
    operations: [
        CreateFunction
        Test
        Publish
    ]
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
    ValidateFunctionRequest: ValidateFunctionRequest
    AutocompleteFunctionRequest: AutocompleteFunctionRequest
}

structure ValidateFunctionRequest {
    key: String
    value: Document
}

structure AutocompleteFunctionRequest {
    name: String
    prefix: String
    environment: Document
}

enum FunctionTypes {
    Validation = "VALIDATION"
    Autocomplete = "AUTOCOMPLETE"
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
    runtime_version: String

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

    @required
    @notProperty
    function: String

    @required
    @notProperty
    runtime_version: String
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

@httpError(404)
@error("client")
structure FunctionNotFound {}

// Operations
@http(method: "POST", uri: "/function")
operation CreateFunction {
    input: CreateFunctionRequest
    output: FunctionResponse
}

@readonly
@http(method: "GET", uri: "/function/{function_name}")
operation GetFunction {
    input := for Function with [WorkspaceMixin] {
        @httpLabel
        @required
        $function_name
    }

    output: FunctionResponse

    errors: [
        FunctionNotFound
    ]
}

@readonly
@http(method: "GET", uri: "/function")
operation ListFunction {
    input := with [PaginationParams, WorkspaceMixin] {}
    output := with [PaginatedResponse] {
        data: FunctionListResponse
    }
}

@idempotent
@http(method: "PATCH", uri: "/function/{function_name}")
operation UpdateFunction {
    input: UpdateFunctionRequest
    output: FunctionResponse
    errors: [
        FunctionNotFound
    ]
}

@idempotent
@http(method: "DELETE", uri: "/function/{function_name}")
operation DeleteFunction {
    input := for Function with [WorkspaceMixin] {
        @httpLabel
        @required
        $function_name
    }

    output := {}

    errors: [
        FunctionNotFound
    ]
}

@idempotent
@http(method: "POST", uri: "/function/{function_name}/{stage}/test")
operation Test {
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

    errors: [
        FunctionNotFound
    ]
}

@idempotent
@http(method: "PATCH", uri: "/function/{function_name}/publish")
operation Publish {
    input := for Function with [WorkspaceMixin] {
        @httpLabel
        @required
        $function_name

        @required
        $change_reason
    }

    output: FunctionResponse

    errors: [
        FunctionNotFound
    ]
}
