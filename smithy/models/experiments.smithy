
$version: "2.0"

namespace io.superposition

use aws.protocols#restJson1

resource Experiments {
    identifiers: {
        id: String
        workspace_id: String
        org_id: String
    }
    properties: {
        created_at: Timestamp
        created_by: String
        last_modified: Timestamp
        name: String
        override_keys: ListOverrideKeys
        status: String
        traffic_percentage: Integer
        context: Document
        variants: ListVariant
        last_modified_by: String
        chosen_variant: String
        description: String
        change_reason: String
    }

    read: GetExperiment
    list: ListExperiment
    create: CreateExperiment
    operations: [ConcludeExperiment, DiscardExperiment, RampExperiment, UpdateOverridesExperiment]

}

list ListOverrideKeys {
    member: String
}

list ListOverride {
    member: Document
}

structure Variant {
    @required
    id: String

    @required
    variant_type: String

    context_id: String
    override_id: String

    @required
    overrides: ListOverride
}

list ListVariant {
    member: Variant
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
    
    @required
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

    //it does not support same field name with to data types, conflicting with resource
    @required
    $variants
    
    $description

    @required
    $change_reason

}


structure CreateExperimentResponse for Experiments{
    @required
    $id
}

list ExperimentList {
    member: ExperimentResponse
}

structure ExperimentListResponse for Experiments{

    @required
    total_pages: Long

    @required
    total_items: Long

    @required
    data: ExperimentList
    
}


@httpError(404)
@error("client")
structure ExperimentNotFound {}


// Operations
@http(method: "POST", uri: "/experiments")
operation CreateExperiment {
    input : CreateExperimentRequest
    output: CreateExperimentResponse
}

// Operations
@http(method: "PUT", uri: "/experiments/{id}/overrides")
operation UpdateOverridesExperiment {
    input : UpdateOverrideRequest
    output: ExperimentResponse
}


@idempotent
@http(method: "PATCH", uri: "/experiments/{id}/conclude")
operation ConcludeExperiment {
    input := for Experiments with [WorkspaceMixin]{
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
    input := for Experiments with [WorkspaceMixin]{
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
    input := for Experiments with [WorkspaceMixin]{
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
    input := for Experiments with [WorkspaceMixin]{
        @httpLabel
        @required
        $id
    }
    output: ExperimentResponse
}

@readonly
@http(method: "GET", uri: "/experiments")
operation ListExperiment {
    input :=  for Experiments with [WorkspaceMixin] {
        @httpQuery("page")
        page: Long

        @httpQuery("count")
        count: Long

        @httpQuery("all")
        all: Boolean
    }

    output : ExperimentListResponse
}

//cannot generate ApplicableVariants
