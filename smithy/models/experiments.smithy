$version: "2.0"

namespace io.superposition

resource Experiments {
    identifiers: {
        workspace_id: String
        org_id: String
    }
    properties: {
        id: String
        created_at: DateTime
        created_by: String
        last_modified: DateTime
        name: String
        override_keys: ListOverrideKeys
        status: ExperimentStatusType
        traffic_percentage: Integer
        context: Condition
        variants: ListVariant
        last_modified_by: String
        chosen_variant: String
        description: String
        change_reason: String
    }
    read: GetExperiment
    operations: [
        ListExperiment
        CreateExperiment
        ConcludeExperiment
        DiscardExperiment
        RampExperiment
        UpdateOverridesExperiment
        ApplicableVariants
    ]
}

list ListOverrideKeys {
    member: String
}

enum ExperimentStatusType {
    CREATED
    CONCLUDED
    INPROGRESS
    DISCARDED
}

enum VariantType {
    CONTROL
    EXPERIMENTAL
}

enum ExperimentSortOn {
    LastModifiedAt = "last_modified_at"
    CreatedAt = "created_at"
}

structure Variant {
    @required
    id: String

    @required
    variant_type: VariantType

    context_id: String

    override_id: String

    @required
    overrides: Document
}

list ListVariant {
    member: Variant
}

structure ApplicableVariantsOutput {
    @required
    @notProperty
    data: ListVariant
}

structure ExperimentResponse for Experiments {
    @required
    $id

    @required
    $created_at

    @required
    $created_by

    @required
    $last_modified

    @required
    $name

    @required
    $override_keys

    @required
    $status

    @required
    $traffic_percentage

    @required
    $context

    @required
    $variants

    @required
    $last_modified_by

    $chosen_variant

    @required
    $description

    @required
    $change_reason
}

structure CreateExperimentRequest for Experiments with [WorkspaceMixin] {
    @required
    $name

    @required
    $context

    @required
    $variants

    @required
    $description

    @required
    $change_reason
}

structure VariantUpdateRequest {
    @required
    id: String

    @required
    overrides: Document
}

list ListVariantUpdateRequest {
    member: VariantUpdateRequest
}

structure UpdateOverrideRequest for Experiments with [WorkspaceMixin] {
    @httpLabel
    @required
    $id

    // it does not support same field name with to data types, conflicting with resource
    @required
    @notProperty
    variant_list: ListVariantUpdateRequest

    $description

    @required
    $change_reason
}

list ExperimentList {
    member: ExperimentResponse
}

structure ExperimentListResponse for Experiments {
    @required
    @notProperty
    total_pages: Long

    @required
    @notProperty
    total_items: Long

    @required
    @notProperty
    data: ExperimentList
}

structure ApplicableVariantsInput for Experiments with [WorkspaceMixin] {
    @required
    $context

    @required
    @notProperty
    toss: Integer
}

@httpError(404)
@error("client")
structure ExperimentNotFound {}

// Operations
@http(method: "POST", uri: "/experiments")
operation CreateExperiment {
    input: CreateExperimentRequest
    output: ExperimentResponse
}

// Operations
@http(method: "PUT", uri: "/experiments/{id}/overrides")
operation UpdateOverridesExperiment {
    input: UpdateOverrideRequest
    output: ExperimentResponse
}

@idempotent
@http(method: "PATCH", uri: "/experiments/{id}/conclude")
operation ConcludeExperiment {
    input := for Experiments with [WorkspaceMixin] {
        @httpLabel
        @required
        $id

        @required
        $chosen_variant

        $description

        @required
        $change_reason
    }

    output: ExperimentResponse
}

@idempotent
@http(method: "PATCH", uri: "/experiments/{id}/discard")
operation DiscardExperiment {
    input := for Experiments with [WorkspaceMixin] {
        @httpLabel
        @required
        $id

        @required
        $change_reason
    }

    output: ExperimentResponse
}

@idempotent
@http(method: "PATCH", uri: "/experiments/{id}/ramp")
operation RampExperiment {
    input := for Experiments with [WorkspaceMixin] {
        @httpLabel
        @required
        $id

        @required
        $change_reason

        @required
        $traffic_percentage
    }

    output: ExperimentResponse
}

@readonly
@http(method: "GET", uri: "/experiments/{id}")
operation GetExperiment {
    input := for Experiments with [WorkspaceMixin] {
        @httpLabel
        @required
        $id
    }

    output: ExperimentResponse
}

@readonly
@http(method: "GET", uri: "/experiments")
operation ListExperiment {
    input := with [WorkspaceMixin] {
        @httpQuery("page")
        @notProperty
        page: Long

        @httpQuery("count")
        @notProperty
        count: Long

        @httpQuery("all")
        @notProperty
        all: Boolean

        @httpQuery("status")
        status: ExperimentStatusType

        @httpQuery("from_date")
        @notProperty
        from_date: DateTime

        @httpQuery("to_date")
        @notProperty
        to_date: DateTime

        @httpQuery("experiment_name")
        @notProperty
        experiment_name: String

        @httpQuery("experiment_ids")
        @notProperty
        experiment_ids: String

        @httpQuery("created_by")
        @notProperty
        created_by: String

        @httpQuery("context")
        @notProperty
        context_query: String

        @httpQuery("sort_on")
        @notProperty        
        sort_on: ExperimentSortOn,

        @httpQuery("sort_by")
        @notProperty     
        sort_by: SortBy
    }

    output: ExperimentListResponse
}

@http(method: "POST", uri: "/experiments/applicable-variants")
operation ApplicableVariants {
    input: ApplicableVariantsInput
    output: ApplicableVariantsOutput
}
