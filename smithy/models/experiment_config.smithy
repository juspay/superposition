$version: "2.0"

namespace io.superposition

@documentation("Represents a configuration of experiments that can be managed together.")
resource ExperimentConfig {
    identifiers: {
        workspace_id: String
        org_id: String
    }
    properties: {
        experiment_groups: ExperimentGroupList
        experiments: ExperimentList
        last_modified: DateTime
    }
    operations: [
        GetExperimentConfig
    ]
}

@documentation("Retrieves the experiment configuration for a given workspace and organization. The response includes details of all experiment groups and experiments that match the specified filters.")
@http(method: "POST", uri: "/experiment-config")
@tags(["Experiment Config"])
operation GetExperimentConfig {
    input := with [WorkspaceMixin] {
        @documentation("While using this, 304 response is treated as error, which needs to be handled separately by checking the response code of the http response. This is required to make sure that clients can cache the response and avoid unnecessary calls when there are no updates.")
        @httpHeader("if-modified-since")
        @notProperty
        if_modified_since: DateTime

        @httpQuery("prefix")
        @notProperty
        prefix: StringList

        @notProperty
        context: ContextMap

        @httpQuery("dimension_match_strategy")
        @notProperty
        dimension_match_strategy: DimensionMatchStrategy
    }

    output := for ExperimentConfig {
        @httpHeader("last-modified")
        @required
        $last_modified

        @required
        $experiments

        @required
        $experiment_groups
    }
}
