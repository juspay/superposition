$version: "2.0"

namespace io.superposition

enum ExperimentGroupSortOn {
    @documentation("Sort by name.")
    Name = "name"

    @documentation("Sort by creation timestamp.")
    CreatedAt = "created_at"

    @documentation("Sort by last modification timestamp.")
    LastModifiedAt = "last_modified_at"
}

@httpError(404)
@error("client")
structure ExperimentGroupNotFound {}


@documentation("Represents a group of experiments that can be managed together.")
resource ExperimentGroup {
    identifiers: {
        workspace_id: String
        org_id: String
    }
    properties: {
        id: String
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
    }
    operations: [
        ListExperimentGroups,
        CreateExperimentGroup,
        GetExperimentGroup,
        UpdateExperimentGroup,
        DeleteExperimentGroup,
        AddMembersToGroup,
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
operation AddMembersToGroup {
    input: ModifyMembersToGroupRequest
    output: ExperimentGroupResponse
    errors: [ResourceNotFound]
}

@documentation("Removes members from an existing experiment group.")
@http(method: "PATCH", uri: "/experiment-groups/{id}/remove-members")
operation RemoveMembersFromGroup {
    input: ModifyMembersToGroupRequest
    output: ExperimentGroupResponse
    errors: [ResourceNotFound]
}

@documentation("Creates a new experiment group.")
@http(method: "POST", uri: "/experiment-groups")
operation CreateExperimentGroup {
    input: CreateExperimentGroupRequest
    output: ExperimentGroupResponse
}

@documentation("Retrieves an existing experiment group by its ID.")
@readonly
@http(method: "GET", uri: "/experiment-groups/{id}")
operation GetExperimentGroup {
    input := for ExperimentGroup with [WorkspaceMixin] {
        @httpLabel
        @required
        $id
    }
    output: ExperimentGroupResponse
    errors: [ResourceNotFound]
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
@http(method: "PATCH", uri: "/experiment-groups/{id}")
operation UpdateExperimentGroup {
    input: UpdateExperimentGroupRequest
    output: ExperimentGroupResponse
    errors: [ResourceNotFound]
}

@documentation("Deletes an experiment group.")
@http(method: "DELETE", uri: "/experiment-groups/{id}")
operation DeleteExperimentGroup {
    input := for ExperimentGroup with [WorkspaceMixin] {
        @httpLabel
        @required
        $id
    }
    output: ExperimentGroupResponse
    errors: [
        ResourceNotFound
    ]
}

@documentation("A list of experiment group responses.")
list ExperimentGroupList {
    member: ExperimentGroupResponse
}

@documentation("Output structure for the list experiment groups operation, including pagination details.")
structure ListExperimentGroupsResponse for ExperimentGroup {
    @required
    @notProperty
    total_pages: Long

    @required
    @notProperty
    total_items: Long

    @required
    @notProperty
    data: ExperimentGroupList
}

@documentation("Lists experiment groups, with support for filtering and pagination.")
@readonly
@http(method: "GET", uri: "/experiment-groups")
operation ListExperimentGroups {
    input := with [WorkspaceMixin] {
        @httpQuery("page")
        @notProperty
        page: Long

        @httpQuery("count")
        @notProperty
        count: Long

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

        @httpQuery("all")
        @documentation("If true, returns all experiment groups, ignoring pagination parameters page and count.")
        @notProperty
        all: Boolean
    }
    output: ListExperimentGroupsResponse
}
