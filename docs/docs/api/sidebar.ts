import type { SidebarsConfig } from "@docusaurus/plugin-content-docs";

const sidebar: SidebarsConfig = {
  apisidebar: [
    {
      type: "doc",
      id: "api/superposition",
    },
    {
      type: "category",
      label: "Configuration Management",
      items: [
        {
          type: "doc",
          id: "api/get-config",
          label: "GetConfig",
          className: "api-method post",
        },
        {
          type: "doc",
          id: "api/get-config-fast",
          label: "GetConfigFast",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/get-resolved-config",
          label: "GetResolvedConfig",
          className: "api-method post",
        },
        {
          type: "doc",
          id: "api/list-versions",
          label: "ListVersions",
          className: "api-method get",
        },
      ],
    },
    {
      type: "category",
      label: "Context Management",
      items: [
        {
          type: "doc",
          id: "api/list-contexts",
          label: "ListContexts",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/create-context",
          label: "CreateContext",
          className: "api-method put",
        },
        {
          type: "doc",
          id: "api/get-context",
          label: "GetContext",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/get-context-from-condition",
          label: "GetContextFromCondition",
          className: "api-method post",
        },
        {
          type: "doc",
          id: "api/move-context",
          label: "MoveContext",
          className: "api-method put",
        },
        {
          type: "doc",
          id: "api/delete-context",
          label: "DeleteContext",
          className: "api-method delete",
        },
        {
          type: "doc",
          id: "api/update-override",
          label: "UpdateOverride",
          className: "api-method put",
        },
        {
          type: "doc",
          id: "api/bulk-operation",
          label: "BulkOperation",
          className: "api-method put",
        },
        {
          type: "doc",
          id: "api/weight-recompute",
          label: "WeightRecompute",
          className: "api-method put",
        },
      ],
    },
    {
      type: "category",
      label: "Default Configuration",
      items: [
        {
          type: "doc",
          id: "api/list-default-configs",
          label: "ListDefaultConfigs",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/create-default-config",
          label: "CreateDefaultConfig",
          className: "api-method post",
        },
        {
          type: "doc",
          id: "api/update-default-config",
          label: "UpdateDefaultConfig",
          className: "api-method put",
        },
        {
          type: "doc",
          id: "api/delete-default-config",
          label: "DeleteDefaultConfig",
          className: "api-method delete",
        },
      ],
    },
    {
      type: "category",
      label: "Dimensions",
      items: [
        {
          type: "doc",
          id: "api/list-dimensions",
          label: "ListDimensions",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/create-dimension",
          label: "CreateDimension",
          className: "api-method post",
        },
        {
          type: "doc",
          id: "api/get-dimension",
          label: "GetDimension",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/update-dimension",
          label: "UpdateDimension",
          className: "api-method put",
        },
        {
          type: "doc",
          id: "api/delete-dimension",
          label: "DeleteDimension",
          className: "api-method delete",
        },
      ],
    },
    {
      type: "category",
      label: "Experimentation",
      items: [
        {
          type: "doc",
          id: "api/list-experiment",
          label: "ListExperiment",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/create-experiment",
          label: "CreateExperiment",
          className: "api-method post",
        },
        {
          type: "doc",
          id: "api/get-experiment",
          label: "GetExperiment",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/applicable-variants",
          label: "ApplicableVariants",
          className: "api-method post",
        },
        {
          type: "doc",
          id: "api/update-overrides-experiment",
          label: "UpdateOverridesExperiment",
          className: "api-method put",
        },
        {
          type: "doc",
          id: "api/conclude-experiment",
          label: "ConcludeExperiment",
          className: "api-method patch",
        },
        {
          type: "doc",
          id: "api/discard-experiment",
          label: "DiscardExperiment",
          className: "api-method patch",
        },
        {
          type: "doc",
          id: "api/pause-experiment",
          label: "PauseExperiment",
          className: "api-method patch",
        },
        {
          type: "doc",
          id: "api/resume-experiment",
          label: "ResumeExperiment",
          className: "api-method patch",
        },
        {
          type: "doc",
          id: "api/ramp-experiment",
          label: "RampExperiment",
          className: "api-method patch",
        },
      ],
    },
    {
      type: "category",
      label: "Experiment Groups",
      items: [
        {
          type: "doc",
          id: "api/list-experiment-groups",
          label: "ListExperimentGroups",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/create-experiment-group",
          label: "CreateExperimentGroup",
          className: "api-method post",
        },
        {
          type: "doc",
          id: "api/get-experiment-group",
          label: "GetExperimentGroup",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/update-experiment-group",
          label: "UpdateExperimentGroup",
          className: "api-method patch",
        },
        {
          type: "doc",
          id: "api/delete-experiment-group",
          label: "DeleteExperimentGroup",
          className: "api-method delete",
        },
        {
          type: "doc",
          id: "api/add-members-to-group",
          label: "AddMembersToGroup",
          className: "api-method patch",
        },
        {
          type: "doc",
          id: "api/remove-members-from-group",
          label: "RemoveMembersFromGroup",
          className: "api-method patch",
        },
      ],
    },
    {
      type: "category",
      label: "Functions",
      items: [
        {
          type: "doc",
          id: "api/list-function",
          label: "ListFunction",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/create-function",
          label: "CreateFunction",
          className: "api-method post",
        },
        {
          type: "doc",
          id: "api/get-function",
          label: "GetFunction",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/update-function",
          label: "UpdateFunction",
          className: "api-method patch",
        },
        {
          type: "doc",
          id: "api/delete-function",
          label: "DeleteFunction",
          className: "api-method delete",
        },
        {
          type: "doc",
          id: "api/test",
          label: "Test",
          className: "api-method post",
        },
        {
          type: "doc",
          id: "api/publish",
          label: "Publish",
          className: "api-method patch",
        },
      ],
    },
    {
      type: "category",
      label: "Organization Management",
      items: [
        {
          type: "doc",
          id: "api/list-organisation",
          label: "ListOrganisation",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/create-organisation",
          label: "CreateOrganisation",
          className: "api-method post",
        },
        {
          type: "doc",
          id: "api/get-organisation",
          label: "GetOrganisation",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/update-organisation",
          label: "UpdateOrganisation",
          className: "api-method put",
        },
      ],
    },
    {
      type: "category",
      label: "Type Templates",
      items: [
        {
          type: "doc",
          id: "api/get-type-templates-list",
          label: "GetTypeTemplatesList",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/create-type-templates",
          label: "CreateTypeTemplates",
          className: "api-method post",
        },
        {
          type: "doc",
          id: "api/update-type-templates",
          label: "UpdateTypeTemplates",
          className: "api-method put",
        },
        {
          type: "doc",
          id: "api/delete-type-templates",
          label: "DeleteTypeTemplates",
          className: "api-method delete",
        },
      ],
    },
    {
      type: "category",
      label: "Webhooks",
      items: [
        {
          type: "doc",
          id: "api/list-webhook",
          label: "ListWebhook",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/create-webhook",
          label: "CreateWebhook",
          className: "api-method post",
        },
        {
          type: "doc",
          id: "api/get-webhook",
          label: "GetWebhook",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/update-webhook",
          label: "UpdateWebhook",
          className: "api-method patch",
        },
      ],
    },
    {
      type: "category",
      label: "Workspace Management",
      items: [
        {
          type: "doc",
          id: "api/list-workspace",
          label: "ListWorkspace",
          className: "api-method get",
        },
        {
          type: "doc",
          id: "api/create-workspace",
          label: "CreateWorkspace",
          className: "api-method post",
        },
        {
          type: "doc",
          id: "api/update-workspace",
          label: "UpdateWorkspace",
          className: "api-method put",
        },
        {
          type: "doc",
          id: "api/migrate-workspace-schema",
          label: "MigrateWorkspaceSchema",
          className: "api-method post",
        },
      ],
    },
    {
      type: "category",
      label: "Audit & Monitoring",
      items: [
        {
          type: "doc",
          id: "api/list-audit-logs",
          label: "ListAuditLogs",
          className: "api-method get",
        },
      ],
    },
  ],
};

export default sidebar.apisidebar;
