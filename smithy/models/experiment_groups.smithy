$version: "2.0"

namespace io.superposition

enum ExperimentGroupSortOn {
    @documentation("Sort by name.")
    NAME = "name"

    @documentation("Sort by creation timestamp.")
    CREATED_AT = "created_at"

    @documentation("Sort by last modification timestamp.")
    LAST_MODIFIED_AT = "last_modified_at"
}

enum GroupType {
    USER_CREATED
    SYSTEM_GENERATED
}

list GroupTypeList {
    member: GroupType
}

structure Bucket {
    @required
    experiment_id: String

    @required
    variant_id: String
}

@sparse
list Buckets {
    member: Bucket
}

@documentation("Represents a group of experiments that can be managed together.")
resource ExperimentGroup {
    identifiers: {
        workspace_id: String
        org_id: String
        id: String
    }
    properties: {
        context_hash: String
        name: String
        description: String
        change_reason: String
        context: Condition
        traffic_percentage: Integer
        member_experiment_ids: StringList
        created_at: DateTime
        created_by: String
        last_modified_at: DateTime
        last_modified_by: String
        buckets: Buckets
        group_type: GroupType
    }
    create: CreateExperimentGroup
    update: UpdateExperimentGroup
    delete: DeleteExperimentGroup
    read: GetExperimentGroup
    list: ListExperimentGroups
    operations: [
        AddMembersToGroup
        RemoveMembersFromGroup
    ]
}

@documentation("Standard response structure for an experiment group.")
structure ExperimentGroupResponse for ExperimentGroup {
    @required
    $id

    @required
    $context_hash

    @required
    $name

    @required
    $description

    @required
    $change_reason

    @required
    $context

    @required
    @range(min: 0, max: 100)
    $traffic_percentage

    @required
    $member_experiment_ids

    @required
    $created_at

    @required
    $created_by

    @required
    $last_modified_at

    @required
    $last_modified_by

    @required
    $buckets

    @required
    $group_type
}

@documentation("Input structure for creating a new experiment group.")
structure CreateExperimentGroupRequest for ExperimentGroup with [WorkspaceMixin] {
    @required
    $name

    @required
    $description

    @required
    @documentation("Reason for creating this experiment group.")
    $change_reason

    @required
    $context

    @required
    @range(min: 0, max: 100)
    $traffic_percentage

    @documentation("List of experiment IDs that are members of this group.")
    $member_experiment_ids
}

@documentation("Input structure for adding members to an experiment group.")
structure ModifyMembersToGroupRequest for ExperimentGroup with [WorkspaceMixin] {
    @httpLabel
    @required
    $id

    @required
    @documentation("Reason for adding these members.")
    $change_reason

    @required
    @documentation("List of experiment IDs to add to this group.")
    $member_experiment_ids
}

@documentation("Adds members to an existing experiment group.")
@http(method: "PATCH", uri: "/experiment-groups/{id}/add-members")
@tags(["Experiment Groups"])
operation AddMembersToGroup with [GetOperation] {
    input: ModifyMembersToGroupRequest
    output: ExperimentGroupResponse
}

@documentation("Removes members from an existing experiment group.")
@http(method: "PATCH", uri: "/experiment-groups/{id}/remove-members")
@tags(["Experiment Groups"])
operation RemoveMembersFromGroup with [GetOperation] {
    input: ModifyMembersToGroupRequest
    output: ExperimentGroupResponse
}

@documentation("Creates a new experiment group.")
@http(method: "POST", uri: "/experiment-groups")
@tags(["Experiment Groups"])
operation CreateExperimentGroup {
    input: CreateExperimentGroupRequest
    output: ExperimentGroupResponse
}

@documentation("Retrieves an existing experiment group by its ID.")
@readonly
@http(method: "GET", uri: "/experiment-groups/{id}")
@tags(["Experiment Groups"])
operation GetExperimentGroup with [GetOperation] {
    input := for ExperimentGroup with [WorkspaceMixin] {
        @httpLabel
        @required
        $id
    }

    output: ExperimentGroupResponse
}

@documentation("Input structure for updating an existing experiment group.")
structure UpdateExperimentGroupRequest for ExperimentGroup with [WorkspaceMixin] {
    @httpLabel
    @required
    $id

    @required
    @documentation("Reason for this update.")
    $change_reason

    @documentation("Optional new description for the group.")
    $description

    @documentation("Optional new traffic percentage for the group.")
    @range(min: 0, max: 100)
    $traffic_percentage
}

@documentation("Updates an existing experiment group. Allows partial updates to specified fields.")
@idempotent
@http(method: "PATCH", uri: "/experiment-groups/{id}")
@tags(["Experiment Groups"])
operation UpdateExperimentGroup with [GetOperation] {
    input: UpdateExperimentGroupRequest
    output: ExperimentGroupResponse
}

@documentation("Deletes an experiment group.")
@idempotent
@http(method: "DELETE", uri: "/experiment-groups/{id}")
@tags(["Experiment Groups"])
operation DeleteExperimentGroup with [GetOperation] {
    input := for ExperimentGroup with [WorkspaceMixin] {
        @httpLabel
        @required
        $id
    }

    output: ExperimentGroupResponse
}

@documentation("A list of experiment group responses.")
list ExperimentGroupList {
    member: ExperimentGroupResponse
}

@documentation("Lists experiment groups, with support for filtering and pagination.")
@readonly
@http(method: "GET", uri: "/experiment-groups")
@tags(["Experiment Groups"])
operation ListExperimentGroups {
    input := with [PaginationParams, WorkspaceMixin] {
        @httpQuery("name")
        @documentation("Filter by experiment group name (exact match or substring, depending on backend implementation).")
        name: String

        @httpQuery("created_by")
        @documentation("Filter by the user who created the experiment group.")
        created_by: String

        @httpQuery("last_modified_by")
        @documentation("Filter by the user who last modified the experiment group.")
        last_modified_by: String

        @httpQuery("sort_on")
        @documentation("Field to sort the results by.")
        @notProperty
        sort_on: ExperimentGroupSortOn

        @httpQuery("sort_by")
        @documentation("Sort order (ascending or descending).")
        @notProperty
        sort_by: SortBy

        @httpQuery("group_type")
        @documentation("Filter by the type of group (USER_CREATED or SYSTEM_GENERATED).")
        group_type: GroupTypeList
    }

    output := with [PaginatedResponse] {
        data: ExperimentGroupList
    }
}
