"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.se_ResumeExperimentCommand = exports.se_RemoveMembersFromGroupCommand = exports.se_RampExperimentCommand = exports.se_PublishCommand = exports.se_PauseExperimentCommand = exports.se_MoveContextCommand = exports.se_ListWorkspaceCommand = exports.se_ListWebhookCommand = exports.se_ListVersionsCommand = exports.se_ListOrganisationCommand = exports.se_ListFunctionCommand = exports.se_ListExperimentGroupsCommand = exports.se_ListExperimentCommand = exports.se_ListDimensionsCommand = exports.se_ListDefaultConfigsCommand = exports.se_ListContextsCommand = exports.se_ListAuditLogsCommand = exports.se_GetWebhookCommand = exports.se_GetTypeTemplatesListCommand = exports.se_GetResolvedConfigCommand = exports.se_GetOrganisationCommand = exports.se_GetFunctionCommand = exports.se_GetExperimentGroupCommand = exports.se_GetExperimentCommand = exports.se_GetDimensionCommand = exports.se_GetContextFromConditionCommand = exports.se_GetContextCommand = exports.se_GetConfigFastCommand = exports.se_GetConfigCommand = exports.se_DiscardExperimentCommand = exports.se_DeleteTypeTemplatesCommand = exports.se_DeleteFunctionCommand = exports.se_DeleteExperimentGroupCommand = exports.se_DeleteDimensionCommand = exports.se_DeleteDefaultConfigCommand = exports.se_DeleteContextCommand = exports.se_CreateWorkspaceCommand = exports.se_CreateWebhookCommand = exports.se_CreateTypeTemplatesCommand = exports.se_CreateOrganisationCommand = exports.se_CreateFunctionCommand = exports.se_CreateExperimentGroupCommand = exports.se_CreateExperimentCommand = exports.se_CreateDimensionCommand = exports.se_CreateDefaultConfigCommand = exports.se_CreateContextCommand = exports.se_ConcludeExperimentCommand = exports.se_BulkOperationCommand = exports.se_ApplicableVariantsCommand = exports.se_AddMembersToGroupCommand = void 0;
exports.de_ListExperimentCommand = exports.de_ListDimensionsCommand = exports.de_ListDefaultConfigsCommand = exports.de_ListContextsCommand = exports.de_ListAuditLogsCommand = exports.de_GetWebhookCommand = exports.de_GetTypeTemplatesListCommand = exports.de_GetResolvedConfigCommand = exports.de_GetOrganisationCommand = exports.de_GetFunctionCommand = exports.de_GetExperimentGroupCommand = exports.de_GetExperimentCommand = exports.de_GetDimensionCommand = exports.de_GetContextFromConditionCommand = exports.de_GetContextCommand = exports.de_GetConfigFastCommand = exports.de_GetConfigCommand = exports.de_DiscardExperimentCommand = exports.de_DeleteTypeTemplatesCommand = exports.de_DeleteFunctionCommand = exports.de_DeleteExperimentGroupCommand = exports.de_DeleteDimensionCommand = exports.de_DeleteDefaultConfigCommand = exports.de_DeleteContextCommand = exports.de_CreateWorkspaceCommand = exports.de_CreateWebhookCommand = exports.de_CreateTypeTemplatesCommand = exports.de_CreateOrganisationCommand = exports.de_CreateFunctionCommand = exports.de_CreateExperimentGroupCommand = exports.de_CreateExperimentCommand = exports.de_CreateDimensionCommand = exports.de_CreateDefaultConfigCommand = exports.de_CreateContextCommand = exports.de_ConcludeExperimentCommand = exports.de_BulkOperationCommand = exports.de_ApplicableVariantsCommand = exports.de_AddMembersToGroupCommand = exports.se_WeightRecomputeCommand = exports.se_UpdateWorkspaceCommand = exports.se_UpdateWebhookCommand = exports.se_UpdateTypeTemplatesCommand = exports.se_UpdateOverridesExperimentCommand = exports.se_UpdateOverrideCommand = exports.se_UpdateOrganisationCommand = exports.se_UpdateFunctionCommand = exports.se_UpdateExperimentGroupCommand = exports.se_UpdateDimensionCommand = exports.se_UpdateDefaultConfigCommand = exports.se_TestCommand = void 0;
exports.de_WeightRecomputeCommand = exports.de_UpdateWorkspaceCommand = exports.de_UpdateWebhookCommand = exports.de_UpdateTypeTemplatesCommand = exports.de_UpdateOverridesExperimentCommand = exports.de_UpdateOverrideCommand = exports.de_UpdateOrganisationCommand = exports.de_UpdateFunctionCommand = exports.de_UpdateExperimentGroupCommand = exports.de_UpdateDimensionCommand = exports.de_UpdateDefaultConfigCommand = exports.de_TestCommand = exports.de_ResumeExperimentCommand = exports.de_RemoveMembersFromGroupCommand = exports.de_RampExperimentCommand = exports.de_PublishCommand = exports.de_PauseExperimentCommand = exports.de_MoveContextCommand = exports.de_ListWorkspaceCommand = exports.de_ListWebhookCommand = exports.de_ListVersionsCommand = exports.de_ListOrganisationCommand = exports.de_ListFunctionCommand = exports.de_ListExperimentGroupsCommand = void 0;
const SuperpositionServiceException_1 = require("../models/SuperpositionServiceException");
const models_0_1 = require("../models/models_0");
const core_1 = require("@aws-sdk/core");
const core_2 = require("@smithy/core");
const smithy_client_1 = require("@smithy/smithy-client");
const se_AddMembersToGroupCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiment-groups/{id}/add-members");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
        'member_experiment_ids': _ => (0, smithy_client_1._json)(_),
    }));
    b.m("PATCH")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_AddMembersToGroupCommand = se_AddMembersToGroupCommand;
const se_ApplicableVariantsCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments/applicable-variants");
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'context': _ => se_Condition(_, context),
        'toss': [],
    }));
    b.m("POST")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_ApplicableVariantsCommand = se_ApplicableVariantsCommand;
const se_BulkOperationCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
        [_xct]: input[_ct],
    });
    b.bp("/context/bulk-operations");
    let body;
    if (input.bulk_operation !== undefined) {
        body = se_BulkOperationReq(input.bulk_operation, context);
    }
    if (body === undefined) {
        body = {};
    }
    body = JSON.stringify(body);
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_BulkOperationCommand = se_BulkOperationCommand;
const se_ConcludeExperimentCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments/{id}/conclude");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
        'chosen_variant': [],
        'description': [],
    }));
    b.m("PATCH")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_ConcludeExperimentCommand = se_ConcludeExperimentCommand;
const se_CreateContextCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
        [_xct]: input[_ct],
    });
    b.bp("/context");
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
        'context': _ => se_Condition(_, context),
        'description': [],
        'override': _ => se_Overrides(_, context),
    }));
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_CreateContextCommand = se_CreateContextCommand;
const se_CreateDefaultConfigCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/default-config");
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'autocomplete_function_name': [],
        'change_reason': [],
        'description': [],
        'function_name': [],
        'key': [],
        'schema': _ => se_Document(_, context),
        'value': _ => se_Document(_, context),
    }));
    b.m("POST")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_CreateDefaultConfigCommand = se_CreateDefaultConfigCommand;
const se_CreateDimensionCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/dimension");
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'autocomplete_function_name': [],
        'change_reason': [],
        'dependencies': _ => (0, smithy_client_1._json)(_),
        'description': [],
        'dimension': [],
        'function_name': [],
        'position': [],
        'schema': _ => se_Document(_, context),
    }));
    b.m("POST")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_CreateDimensionCommand = se_CreateDimensionCommand;
const se_CreateExperimentCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments");
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
        'context': _ => se_Condition(_, context),
        'description': [],
        'experiment_group_id': [],
        'experiment_type': [],
        'metrics': _ => se_Document(_, context),
        'name': [],
        'variants': _ => se_ListVariant(_, context),
    }));
    b.m("POST")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_CreateExperimentCommand = se_CreateExperimentCommand;
const se_CreateExperimentGroupCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiment-groups");
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
        'context': _ => se_Condition(_, context),
        'description': [],
        'member_experiment_ids': _ => (0, smithy_client_1._json)(_),
        'name': [],
        'traffic_percentage': [],
    }));
    b.m("POST")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_CreateExperimentGroupCommand = se_CreateExperimentGroupCommand;
const se_CreateFunctionCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/function");
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
        'description': [],
        'function': [],
        'function_name': [],
        'function_type': [],
        'runtime_version': [],
    }));
    b.m("POST")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_CreateFunctionCommand = se_CreateFunctionCommand;
const se_CreateOrganisationCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = {
        'content-type': 'application/json',
    };
    b.bp("/superposition/organisations");
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'admin_email': [],
        'contact_email': [],
        'contact_phone': [],
        'country_code': [],
        'name': [],
        'sector': [],
    }));
    b.m("POST")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_CreateOrganisationCommand = se_CreateOrganisationCommand;
const se_CreateTypeTemplatesCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/types");
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
        'description': [],
        'type_name': [],
        'type_schema': _ => se_Document(_, context),
    }));
    b.m("POST")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_CreateTypeTemplatesCommand = se_CreateTypeTemplatesCommand;
const se_CreateWebhookCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/webhook");
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
        'custom_headers': _ => se_Object(_, context),
        'description': [],
        'enabled': [],
        'events': _ => (0, smithy_client_1._json)(_),
        'method': [],
        'name': [],
        'url': [],
        'version': [],
    }));
    b.m("POST")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_CreateWebhookCommand = se_CreateWebhookCommand;
const se_CreateWorkspaceCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xoi]: input[_oi],
    });
    b.bp("/workspaces");
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'allow_experiment_self_approval': [],
        'metrics': _ => se_Document(_, context),
        'strict_mode': [],
        'workspace_admin_email': [],
        'workspace_name': [],
        'workspace_status': [],
    }));
    b.m("POST")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_CreateWorkspaceCommand = se_CreateWorkspaceCommand;
const se_DeleteContextCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
        [_xct]: input[_ct],
    });
    b.bp("/context/{id}");
    b.p('id', () => input.id, '{id}', false);
    let body;
    b.m("DELETE")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_DeleteContextCommand = se_DeleteContextCommand;
const se_DeleteDefaultConfigCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/default-config/{key}");
    b.p('key', () => input.key, '{key}', false);
    let body;
    b.m("DELETE")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_DeleteDefaultConfigCommand = se_DeleteDefaultConfigCommand;
const se_DeleteDimensionCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/dimension/{dimension}");
    b.p('dimension', () => input.dimension, '{dimension}', false);
    let body;
    b.m("DELETE")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_DeleteDimensionCommand = se_DeleteDimensionCommand;
const se_DeleteExperimentGroupCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiment-groups/{id}");
    b.p('id', () => input.id, '{id}', false);
    let body;
    b.m("DELETE")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_DeleteExperimentGroupCommand = se_DeleteExperimentGroupCommand;
const se_DeleteFunctionCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/function/{function_name}");
    b.p('function_name', () => input.function_name, '{function_name}', false);
    let body;
    b.m("DELETE")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_DeleteFunctionCommand = se_DeleteFunctionCommand;
const se_DeleteTypeTemplatesCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/types/{type_name}");
    b.p('type_name', () => input.type_name, '{type_name}', false);
    let body;
    b.m("DELETE")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_DeleteTypeTemplatesCommand = se_DeleteTypeTemplatesCommand;
const se_DiscardExperimentCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments/{id}/discard");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
    }));
    b.m("PATCH")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_DiscardExperimentCommand = se_DiscardExperimentCommand;
const se_GetConfigCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/config");
    const query = (0, smithy_client_1.map)({
        [_p]: [, input[_p]],
        [_v]: [, input[_v]],
    });
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'context': _ => se_ContextMap(_, context),
    }));
    b.m("POST")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
exports.se_GetConfigCommand = se_GetConfigCommand;
const se_GetConfigFastCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/config/fast");
    let body;
    b.m("GET")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_GetConfigFastCommand = se_GetConfigFastCommand;
const se_GetContextCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/context/{id}");
    b.p('id', () => input.id, '{id}', false);
    let body;
    b.m("GET")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_GetContextCommand = se_GetContextCommand;
const se_GetContextFromConditionCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/context/get");
    let body;
    if (input.context !== undefined) {
        if (input.context === null) {
            body = "null";
        }
        else {
            body = input.context;
        }
    }
    body = JSON.stringify(body);
    b.m("POST")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_GetContextFromConditionCommand = se_GetContextFromConditionCommand;
const se_GetDimensionCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/dimension/{dimension}");
    b.p('dimension', () => input.dimension, '{dimension}', false);
    let body;
    b.m("GET")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_GetDimensionCommand = se_GetDimensionCommand;
const se_GetExperimentCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments/{id}");
    b.p('id', () => input.id, '{id}', false);
    let body;
    b.m("GET")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_GetExperimentCommand = se_GetExperimentCommand;
const se_GetExperimentGroupCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiment-groups/{id}");
    b.p('id', () => input.id, '{id}', false);
    let body;
    b.m("GET")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_GetExperimentGroupCommand = se_GetExperimentGroupCommand;
const se_GetFunctionCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/function/{function_name}");
    b.p('function_name', () => input.function_name, '{function_name}', false);
    let body;
    b.m("GET")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_GetFunctionCommand = se_GetFunctionCommand;
const se_GetOrganisationCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = {};
    b.bp("/superposition/organisations/{id}");
    b.p('id', () => input.id, '{id}', false);
    let body;
    b.m("GET")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_GetOrganisationCommand = se_GetOrganisationCommand;
const se_GetResolvedConfigCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
        [_xms]: input[_ms],
    });
    b.bp("/config/resolve");
    const query = (0, smithy_client_1.map)({
        [_p]: [, input[_p]],
        [_v]: [, input[_v]],
        [_sr]: [() => input.show_reasoning !== void 0, () => (input[_sr].toString())],
        [_ci]: [, input[_ci]],
    });
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'context': _ => se_ContextMap(_, context),
    }));
    b.m("POST")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
exports.se_GetResolvedConfigCommand = se_GetResolvedConfigCommand;
const se_GetTypeTemplatesListCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/types");
    const query = (0, smithy_client_1.map)({
        [_c]: [() => input.count !== void 0, () => (input[_c].toString())],
        [_pa]: [() => input.page !== void 0, () => (input[_pa].toString())],
        [_a]: [() => input.all !== void 0, () => (input[_a].toString())],
    });
    let body;
    b.m("GET")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
exports.se_GetTypeTemplatesListCommand = se_GetTypeTemplatesListCommand;
const se_GetWebhookCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/webhook/{name}");
    b.p('name', () => input.name, '{name}', false);
    let body;
    b.m("GET")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_GetWebhookCommand = se_GetWebhookCommand;
const se_ListAuditLogsCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/audit");
    const query = (0, smithy_client_1.map)({
        [_c]: [() => input.count !== void 0, () => (input[_c].toString())],
        [_pa]: [() => input.page !== void 0, () => (input[_pa].toString())],
        [_a]: [() => input.all !== void 0, () => (input[_a].toString())],
        [_fd]: [() => input.from_date !== void 0, () => ((0, smithy_client_1.serializeDateTime)(input[_fd]).toString())],
        [_td]: [() => input.to_date !== void 0, () => ((0, smithy_client_1.serializeDateTime)(input[_td]).toString())],
        [_ta]: [, input[_t]],
        [_ac]: [, input[_ac]],
        [_u]: [, input[_u]],
    });
    let body;
    b.m("GET")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
exports.se_ListAuditLogsCommand = se_ListAuditLogsCommand;
const se_ListContextsCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/context/list");
    const query = (0, smithy_client_1.map)({
        [_pa]: [() => input.page !== void 0, () => (input[_pa].toString())],
        [_c]: [() => input.count !== void 0, () => (input[_c].toString())],
        [_p]: [, input[_p]],
        [_so]: [, input[_so]],
        [_sb]: [, input[_sb]],
        [_cb]: [, input[_cb]],
        [_lmb]: [, input[_lmb]],
        [_pl]: [, input[_pl]],
    });
    let body;
    b.m("GET")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
exports.se_ListContextsCommand = se_ListContextsCommand;
const se_ListDefaultConfigsCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/default-config");
    const query = (0, smithy_client_1.map)({
        [_c]: [() => input.count !== void 0, () => (input[_c].toString())],
        [_pa]: [() => input.page !== void 0, () => (input[_pa].toString())],
        [_a]: [() => input.all !== void 0, () => (input[_a].toString())],
    });
    let body;
    b.m("GET")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
exports.se_ListDefaultConfigsCommand = se_ListDefaultConfigsCommand;
const se_ListDimensionsCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/dimension");
    const query = (0, smithy_client_1.map)({
        [_c]: [() => input.count !== void 0, () => (input[_c].toString())],
        [_pa]: [() => input.page !== void 0, () => (input[_pa].toString())],
        [_a]: [() => input.all !== void 0, () => (input[_a].toString())],
    });
    let body;
    b.m("GET")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
exports.se_ListDimensionsCommand = se_ListDimensionsCommand;
const se_ListExperimentCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments");
    const query = (0, smithy_client_1.map)({
        [_pa]: [() => input.page !== void 0, () => (input[_pa].toString())],
        [_c]: [() => input.count !== void 0, () => (input[_c].toString())],
        [_a]: [() => input.all !== void 0, () => (input[_a].toString())],
        [_s]: [, input[_s]],
        [_fd]: [() => input.from_date !== void 0, () => ((0, smithy_client_1.serializeDateTime)(input[_fd]).toString())],
        [_td]: [() => input.to_date !== void 0, () => ((0, smithy_client_1.serializeDateTime)(input[_td]).toString())],
        [_en]: [, input[_en]],
        [_ei]: [, input[_ei]],
        [_egi]: [, input[_egi]],
        [_cb]: [, input[_cb]],
        [_so]: [, input[_so]],
        [_sb]: [, input[_sb]],
    });
    let body;
    b.m("GET")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
exports.se_ListExperimentCommand = se_ListExperimentCommand;
const se_ListExperimentGroupsCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiment-groups");
    const query = (0, smithy_client_1.map)({
        [_pa]: [() => input.page !== void 0, () => (input[_pa].toString())],
        [_c]: [() => input.count !== void 0, () => (input[_c].toString())],
        [_n]: [, input[_n]],
        [_cb]: [, input[_cb]],
        [_lmb]: [, input[_lmb]],
        [_so]: [, input[_so]],
        [_sb]: [, input[_sb]],
        [_a]: [() => input.all !== void 0, () => (input[_a].toString())],
    });
    let body;
    b.m("GET")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
exports.se_ListExperimentGroupsCommand = se_ListExperimentGroupsCommand;
const se_ListFunctionCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/function");
    const query = (0, smithy_client_1.map)({
        [_c]: [() => input.count !== void 0, () => (input[_c].toString())],
        [_pa]: [() => input.page !== void 0, () => (input[_pa].toString())],
        [_a]: [() => input.all !== void 0, () => (input[_a].toString())],
    });
    let body;
    b.m("GET")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
exports.se_ListFunctionCommand = se_ListFunctionCommand;
const se_ListOrganisationCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = {};
    b.bp("/superposition/organisations");
    const query = (0, smithy_client_1.map)({
        [_c]: [() => input.count !== void 0, () => (input[_c].toString())],
        [_pa]: [() => input.page !== void 0, () => (input[_pa].toString())],
        [_a]: [() => input.all !== void 0, () => (input[_a].toString())],
    });
    let body;
    b.m("GET")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
exports.se_ListOrganisationCommand = se_ListOrganisationCommand;
const se_ListVersionsCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/config/versions");
    const query = (0, smithy_client_1.map)({
        [_c]: [() => input.count !== void 0, () => (input[_c].toString())],
        [_pa]: [() => input.page !== void 0, () => (input[_pa].toString())],
    });
    let body;
    b.m("GET")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
exports.se_ListVersionsCommand = se_ListVersionsCommand;
const se_ListWebhookCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/webhook");
    const query = (0, smithy_client_1.map)({
        [_c]: [() => input.count !== void 0, () => (input[_c].toString())],
        [_pa]: [() => input.page !== void 0, () => (input[_pa].toString())],
        [_a]: [() => input.all !== void 0, () => (input[_a].toString())],
    });
    let body;
    b.m("GET")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
exports.se_ListWebhookCommand = se_ListWebhookCommand;
const se_ListWorkspaceCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xoi]: input[_oi],
    });
    b.bp("/workspaces");
    const query = (0, smithy_client_1.map)({
        [_c]: [() => input.count !== void 0, () => (input[_c].toString())],
        [_pa]: [() => input.page !== void 0, () => (input[_pa].toString())],
        [_a]: [() => input.all !== void 0, () => (input[_a].toString())],
    });
    let body;
    b.m("GET")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
exports.se_ListWorkspaceCommand = se_ListWorkspaceCommand;
const se_MoveContextCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/context/move/{id}");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
        'context': _ => se_Condition(_, context),
        'description': [],
    }));
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_MoveContextCommand = se_MoveContextCommand;
const se_PauseExperimentCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments/{id}/pause");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
    }));
    b.m("PATCH")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_PauseExperimentCommand = se_PauseExperimentCommand;
const se_PublishCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/function/{function_name}/publish");
    b.p('function_name', () => input.function_name, '{function_name}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
    }));
    b.m("PATCH")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_PublishCommand = se_PublishCommand;
const se_RampExperimentCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments/{id}/ramp");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
        'traffic_percentage': [],
    }));
    b.m("PATCH")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_RampExperimentCommand = se_RampExperimentCommand;
const se_RemoveMembersFromGroupCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiment-groups/{id}/remove-members");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
        'member_experiment_ids': _ => (0, smithy_client_1._json)(_),
    }));
    b.m("PATCH")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_RemoveMembersFromGroupCommand = se_RemoveMembersFromGroupCommand;
const se_ResumeExperimentCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments/{id}/resume");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
    }));
    b.m("PATCH")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_ResumeExperimentCommand = se_ResumeExperimentCommand;
const se_TestCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/function/{function_name}/{stage}/test");
    b.p('function_name', () => input.function_name, '{function_name}', false);
    b.p('stage', () => input.stage, '{stage}', false);
    let body;
    if (input.request !== undefined) {
        body = se_FunctionExecutionRequest(input.request, context);
    }
    if (body === undefined) {
        body = {};
    }
    body = JSON.stringify(body);
    b.m("POST")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_TestCommand = se_TestCommand;
const se_UpdateDefaultConfigCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/default-config/{key}");
    b.p('key', () => input.key, '{key}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'autocomplete_function_name': [],
        'change_reason': [],
        'description': [],
        'function_name': [],
        'schema': _ => se_Document(_, context),
        'value': _ => se_Document(_, context),
    }));
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_UpdateDefaultConfigCommand = se_UpdateDefaultConfigCommand;
const se_UpdateDimensionCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/dimension/{dimension}");
    b.p('dimension', () => input.dimension, '{dimension}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'autocomplete_function_name': [],
        'change_reason': [],
        'dependencies': _ => (0, smithy_client_1._json)(_),
        'description': [],
        'function_name': [],
        'position': [],
        'schema': _ => se_Document(_, context),
    }));
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_UpdateDimensionCommand = se_UpdateDimensionCommand;
const se_UpdateExperimentGroupCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiment-groups/{id}");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
        'description': [],
        'traffic_percentage': [],
    }));
    b.m("PATCH")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_UpdateExperimentGroupCommand = se_UpdateExperimentGroupCommand;
const se_UpdateFunctionCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/function/{function_name}");
    b.p('function_name', () => input.function_name, '{function_name}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
        'description': [],
        'function': [],
        'runtime_version': [],
    }));
    b.m("PATCH")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_UpdateFunctionCommand = se_UpdateFunctionCommand;
const se_UpdateOrganisationCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = {
        'content-type': 'application/json',
    };
    b.bp("/superposition/organisations/{id}");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'admin_email': [],
        'contact_email': [],
        'contact_phone': [],
        'country_code': [],
        'sector': [],
        'status': [],
    }));
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_UpdateOrganisationCommand = se_UpdateOrganisationCommand;
const se_UpdateOverrideCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
        [_xct]: input[_ct],
    });
    b.bp("/context/overrides");
    let body;
    if (input.request !== undefined) {
        body = se_UpdateContextOverrideRequest(input.request, context);
    }
    if (body === undefined) {
        body = {};
    }
    body = JSON.stringify(body);
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_UpdateOverrideCommand = se_UpdateOverrideCommand;
const se_UpdateOverridesExperimentCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments/{id}/overrides");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
        'description': [],
        'experiment_group_id': [],
        'metrics': _ => se_Document(_, context),
        'variant_list': _ => se_ListVariantUpdateRequest(_, context),
    }));
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_UpdateOverridesExperimentCommand = se_UpdateOverridesExperimentCommand;
const se_UpdateTypeTemplatesCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/types/{type_name}");
    b.p('type_name', () => input.type_name, '{type_name}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
        'description': [],
        'type_schema': _ => se_Document(_, context),
    }));
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_UpdateTypeTemplatesCommand = se_UpdateTypeTemplatesCommand;
const se_UpdateWebhookCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/webhook/{name}");
    b.p('name', () => input.name, '{name}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'change_reason': [],
        'custom_headers': _ => se_Object(_, context),
        'description': [],
        'enabled': [],
        'events': _ => (0, smithy_client_1._json)(_),
        'method': [],
        'url': [],
        'version': [],
    }));
    b.m("PATCH")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_UpdateWebhookCommand = se_UpdateWebhookCommand;
const se_UpdateWorkspaceCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xoi]: input[_oi],
    });
    b.bp("/workspaces/{workspace_name}");
    b.p('workspace_name', () => input.workspace_name, '{workspace_name}', false);
    let body;
    body = JSON.stringify((0, smithy_client_1.take)(input, {
        'allow_experiment_self_approval': [],
        'config_version': [],
        'mandatory_dimensions': _ => (0, smithy_client_1._json)(_),
        'metrics': _ => se_Document(_, context),
        'workspace_admin_email': [],
        'workspace_status': [],
    }));
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_UpdateWorkspaceCommand = se_UpdateWorkspaceCommand;
const se_WeightRecomputeCommand = async (input, context) => {
    const b = (0, core_2.requestBuilder)(input, context);
    const headers = (0, smithy_client_1.map)({}, smithy_client_1.isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
        [_xct]: input[_ct],
    });
    b.bp("/context/weight/recompute");
    let body;
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
exports.se_WeightRecomputeCommand = se_WeightRecomputeCommand;
const de_AddMembersToGroupCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'context': _ => de_Condition(_, context),
        'context_hash': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'member_experiment_ids': smithy_client_1._json,
        'name': smithy_client_1.expectString,
        'traffic_percentage': smithy_client_1.expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_AddMembersToGroupCommand = de_AddMembersToGroupCommand;
const de_ApplicableVariantsCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'data': _ => de_ListVariant(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_ApplicableVariantsCommand = de_ApplicableVariantsCommand;
const de_BulkOperationCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context));
    contents.bulk_operation_output = (0, smithy_client_1._json)(data);
    return contents;
};
exports.de_BulkOperationCommand = de_BulkOperationCommand;
const de_ConcludeExperimentCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'chosen_variant': smithy_client_1.expectString,
        'context': _ => de_Condition(_, context),
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'experiment_group_id': smithy_client_1.expectString,
        'experiment_type': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'metrics': _ => de_Document(_, context),
        'metrics_url': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'override_keys': smithy_client_1._json,
        'started_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'started_by': smithy_client_1.expectString,
        'status': smithy_client_1.expectString,
        'traffic_percentage': smithy_client_1.expectInt32,
        'variants': _ => de_ListVariant(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_ConcludeExperimentCommand = de_ConcludeExperimentCommand;
const de_CreateContextCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'context_id': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'override_id': smithy_client_1.expectString,
        'weight': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_CreateContextCommand = de_CreateContextCommand;
const de_CreateDefaultConfigCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'autocomplete_function_name': smithy_client_1.expectString,
        'change_reason': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'function_name': smithy_client_1.expectString,
        'key': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'schema': _ => de_Document(_, context),
        'value': _ => de_Document(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_CreateDefaultConfigCommand = de_CreateDefaultConfigCommand;
const de_CreateDimensionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'autocomplete_function_name': smithy_client_1.expectString,
        'change_reason': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'dependencies': smithy_client_1._json,
        'dependency_graph': _ => de_Object(_, context),
        'dependents': smithy_client_1._json,
        'description': smithy_client_1.expectString,
        'dimension': smithy_client_1.expectString,
        'function_name': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'mandatory': smithy_client_1.expectBoolean,
        'position': smithy_client_1.expectInt32,
        'schema': _ => de_Document(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_CreateDimensionCommand = de_CreateDimensionCommand;
const de_CreateExperimentCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'chosen_variant': smithy_client_1.expectString,
        'context': _ => de_Condition(_, context),
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'experiment_group_id': smithy_client_1.expectString,
        'experiment_type': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'metrics': _ => de_Document(_, context),
        'metrics_url': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'override_keys': smithy_client_1._json,
        'started_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'started_by': smithy_client_1.expectString,
        'status': smithy_client_1.expectString,
        'traffic_percentage': smithy_client_1.expectInt32,
        'variants': _ => de_ListVariant(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_CreateExperimentCommand = de_CreateExperimentCommand;
const de_CreateExperimentGroupCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'context': _ => de_Condition(_, context),
        'context_hash': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'member_experiment_ids': smithy_client_1._json,
        'name': smithy_client_1.expectString,
        'traffic_percentage': smithy_client_1.expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_CreateExperimentGroupCommand = de_CreateExperimentGroupCommand;
const de_CreateFunctionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'draft_code': smithy_client_1.expectString,
        'draft_edited_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'draft_edited_by': smithy_client_1.expectString,
        'draft_runtime_version': smithy_client_1.expectString,
        'function_name': smithy_client_1.expectString,
        'function_type': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'published_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'published_by': smithy_client_1.expectString,
        'published_code': smithy_client_1.expectString,
        'published_runtime_version': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_CreateFunctionCommand = de_CreateFunctionCommand;
const de_CreateOrganisationCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'admin_email': smithy_client_1.expectString,
        'contact_email': smithy_client_1.expectString,
        'contact_phone': smithy_client_1.expectString,
        'country_code': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'sector': smithy_client_1.expectString,
        'status': smithy_client_1.expectString,
        'updated_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'updated_by': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_CreateOrganisationCommand = de_CreateOrganisationCommand;
const de_CreateTypeTemplatesCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'type_name': smithy_client_1.expectString,
        'type_schema': _ => de_Document(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_CreateTypeTemplatesCommand = de_CreateTypeTemplatesCommand;
const de_CreateWebhookCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'custom_headers': _ => de_Object(_, context),
        'description': smithy_client_1.expectString,
        'enabled': smithy_client_1.expectBoolean,
        'events': smithy_client_1._json,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'last_triggered_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'max_retries': smithy_client_1.expectInt32,
        'method': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'url': smithy_client_1.expectString,
        'version': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_CreateWebhookCommand = de_CreateWebhookCommand;
const de_CreateWorkspaceCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'allow_experiment_self_approval': smithy_client_1.expectBoolean,
        'config_version': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'mandatory_dimensions': smithy_client_1._json,
        'metrics': _ => de_Document(_, context),
        'organisation_id': smithy_client_1.expectString,
        'organisation_name': smithy_client_1.expectString,
        'strict_mode': smithy_client_1.expectBoolean,
        'workspace_admin_email': smithy_client_1.expectString,
        'workspace_name': smithy_client_1.expectString,
        'workspace_schema_name': smithy_client_1.expectString,
        'workspace_status': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_CreateWorkspaceCommand = de_CreateWorkspaceCommand;
const de_DeleteContextCommand = async (output, context) => {
    if (output.statusCode !== 201 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    await (0, smithy_client_1.collectBody)(output.body, context);
    return contents;
};
exports.de_DeleteContextCommand = de_DeleteContextCommand;
const de_DeleteDefaultConfigCommand = async (output, context) => {
    if (output.statusCode !== 201 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    await (0, smithy_client_1.collectBody)(output.body, context);
    return contents;
};
exports.de_DeleteDefaultConfigCommand = de_DeleteDefaultConfigCommand;
const de_DeleteDimensionCommand = async (output, context) => {
    if (output.statusCode !== 201 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    await (0, smithy_client_1.collectBody)(output.body, context);
    return contents;
};
exports.de_DeleteDimensionCommand = de_DeleteDimensionCommand;
const de_DeleteExperimentGroupCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'context': _ => de_Condition(_, context),
        'context_hash': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'member_experiment_ids': smithy_client_1._json,
        'name': smithy_client_1.expectString,
        'traffic_percentage': smithy_client_1.expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_DeleteExperimentGroupCommand = de_DeleteExperimentGroupCommand;
const de_DeleteFunctionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    await (0, smithy_client_1.collectBody)(output.body, context);
    return contents;
};
exports.de_DeleteFunctionCommand = de_DeleteFunctionCommand;
const de_DeleteTypeTemplatesCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'type_name': smithy_client_1.expectString,
        'type_schema': _ => de_Document(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_DeleteTypeTemplatesCommand = de_DeleteTypeTemplatesCommand;
const de_DiscardExperimentCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'chosen_variant': smithy_client_1.expectString,
        'context': _ => de_Condition(_, context),
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'experiment_group_id': smithy_client_1.expectString,
        'experiment_type': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'metrics': _ => de_Document(_, context),
        'metrics_url': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'override_keys': smithy_client_1._json,
        'started_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'started_by': smithy_client_1.expectString,
        'status': smithy_client_1.expectString,
        'traffic_percentage': smithy_client_1.expectInt32,
        'variants': _ => de_ListVariant(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_DiscardExperimentCommand = de_DiscardExperimentCommand;
const de_GetConfigCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
        [_v]: [, output.headers[_xcv]],
        [_lm_]: [() => void 0 !== output.headers[_lm], () => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(output.headers[_lm]))],
        [_ai]: [, output.headers[_xai]],
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'contexts': _ => de_ContextList(_, context),
        'default_configs': _ => de_Object(_, context),
        'overrides': _ => de_OverridesMap(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_GetConfigCommand = de_GetConfigCommand;
const de_GetConfigFastCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
        [_v]: [, output.headers[_xcv]],
        [_lm_]: [() => void 0 !== output.headers[_lm], () => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(output.headers[_lm]))],
        [_ai]: [, output.headers[_xai]],
    });
    const data = await collectBodyString(output.body, context);
    contents.config = data;
    contents.config = JSON.parse(data);
    return contents;
};
exports.de_GetConfigFastCommand = de_GetConfigFastCommand;
const de_GetContextCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'override': _ => de_Overrides(_, context),
        'override_id': smithy_client_1.expectString,
        'value': _ => de_Condition(_, context),
        'weight': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_GetContextCommand = de_GetContextCommand;
const de_GetContextFromConditionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'override': _ => de_Overrides(_, context),
        'override_id': smithy_client_1.expectString,
        'value': _ => de_Condition(_, context),
        'weight': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_GetContextFromConditionCommand = de_GetContextFromConditionCommand;
const de_GetDimensionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'autocomplete_function_name': smithy_client_1.expectString,
        'change_reason': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'dependencies': smithy_client_1._json,
        'dependency_graph': _ => de_Object(_, context),
        'dependents': smithy_client_1._json,
        'description': smithy_client_1.expectString,
        'dimension': smithy_client_1.expectString,
        'function_name': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'mandatory': smithy_client_1.expectBoolean,
        'position': smithy_client_1.expectInt32,
        'schema': _ => de_Document(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_GetDimensionCommand = de_GetDimensionCommand;
const de_GetExperimentCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'chosen_variant': smithy_client_1.expectString,
        'context': _ => de_Condition(_, context),
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'experiment_group_id': smithy_client_1.expectString,
        'experiment_type': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'metrics': _ => de_Document(_, context),
        'metrics_url': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'override_keys': smithy_client_1._json,
        'started_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'started_by': smithy_client_1.expectString,
        'status': smithy_client_1.expectString,
        'traffic_percentage': smithy_client_1.expectInt32,
        'variants': _ => de_ListVariant(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_GetExperimentCommand = de_GetExperimentCommand;
const de_GetExperimentGroupCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'context': _ => de_Condition(_, context),
        'context_hash': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'member_experiment_ids': smithy_client_1._json,
        'name': smithy_client_1.expectString,
        'traffic_percentage': smithy_client_1.expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_GetExperimentGroupCommand = de_GetExperimentGroupCommand;
const de_GetFunctionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'draft_code': smithy_client_1.expectString,
        'draft_edited_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'draft_edited_by': smithy_client_1.expectString,
        'draft_runtime_version': smithy_client_1.expectString,
        'function_name': smithy_client_1.expectString,
        'function_type': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'published_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'published_by': smithy_client_1.expectString,
        'published_code': smithy_client_1.expectString,
        'published_runtime_version': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_GetFunctionCommand = de_GetFunctionCommand;
const de_GetOrganisationCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'admin_email': smithy_client_1.expectString,
        'contact_email': smithy_client_1.expectString,
        'contact_phone': smithy_client_1.expectString,
        'country_code': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'sector': smithy_client_1.expectString,
        'status': smithy_client_1.expectString,
        'updated_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'updated_by': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_GetOrganisationCommand = de_GetOrganisationCommand;
const de_GetResolvedConfigCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
        [_v]: [, output.headers[_xcv]],
        [_lm_]: [() => void 0 !== output.headers[_lm], () => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(output.headers[_lm]))],
        [_ai]: [, output.headers[_xai]],
    });
    const data = await collectBodyString(output.body, context);
    contents.config = data;
    contents.config = JSON.parse(data);
    return contents;
};
exports.de_GetResolvedConfigCommand = de_GetResolvedConfigCommand;
const de_GetTypeTemplatesListCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'data': _ => de_TypeTemplatesList(_, context),
        'total_items': smithy_client_1.expectInt32,
        'total_pages': smithy_client_1.expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_GetTypeTemplatesListCommand = de_GetTypeTemplatesListCommand;
const de_GetWebhookCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'custom_headers': _ => de_Object(_, context),
        'description': smithy_client_1.expectString,
        'enabled': smithy_client_1.expectBoolean,
        'events': smithy_client_1._json,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'last_triggered_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'max_retries': smithy_client_1.expectInt32,
        'method': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'url': smithy_client_1.expectString,
        'version': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_GetWebhookCommand = de_GetWebhookCommand;
const de_ListAuditLogsCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'data': _ => de_AuditLogList(_, context),
        'total_items': smithy_client_1.expectInt32,
        'total_pages': smithy_client_1.expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_ListAuditLogsCommand = de_ListAuditLogsCommand;
const de_ListContextsCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'data': _ => de_ListContextOut(_, context),
        'total_items': smithy_client_1.expectInt32,
        'total_pages': smithy_client_1.expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_ListContextsCommand = de_ListContextsCommand;
const de_ListDefaultConfigsCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'data': _ => de_ListDefaultConfigOut(_, context),
        'total_items': smithy_client_1.expectInt32,
        'total_pages': smithy_client_1.expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_ListDefaultConfigsCommand = de_ListDefaultConfigsCommand;
const de_ListDimensionsCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'data': _ => de_DimensionExtList(_, context),
        'total_items': smithy_client_1.expectInt32,
        'total_pages': smithy_client_1.expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_ListDimensionsCommand = de_ListDimensionsCommand;
const de_ListExperimentCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'data': _ => de_ExperimentList(_, context),
        'total_items': smithy_client_1.expectLong,
        'total_pages': smithy_client_1.expectLong,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_ListExperimentCommand = de_ListExperimentCommand;
const de_ListExperimentGroupsCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'data': _ => de_ExperimentGroupList(_, context),
        'total_items': smithy_client_1.expectLong,
        'total_pages': smithy_client_1.expectLong,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_ListExperimentGroupsCommand = de_ListExperimentGroupsCommand;
const de_ListFunctionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'data': _ => de_FunctionListResponse(_, context),
        'total_items': smithy_client_1.expectInt32,
        'total_pages': smithy_client_1.expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_ListFunctionCommand = de_ListFunctionCommand;
const de_ListOrganisationCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'data': _ => de_OrganisationList(_, context),
        'total_items': smithy_client_1.expectInt32,
        'total_pages': smithy_client_1.expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_ListOrganisationCommand = de_ListOrganisationCommand;
const de_ListVersionsCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'data': _ => de_ListVersionsOut(_, context),
        'total_items': smithy_client_1.expectInt32,
        'total_pages': smithy_client_1.expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_ListVersionsCommand = de_ListVersionsCommand;
const de_ListWebhookCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'data': _ => de_WebhookList(_, context),
        'total_items': smithy_client_1.expectLong,
        'total_pages': smithy_client_1.expectLong,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_ListWebhookCommand = de_ListWebhookCommand;
const de_ListWorkspaceCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'data': _ => de_WorkspaceList(_, context),
        'total_items': smithy_client_1.expectLong,
        'total_pages': smithy_client_1.expectLong,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_ListWorkspaceCommand = de_ListWorkspaceCommand;
const de_MoveContextCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'context_id': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'override_id': smithy_client_1.expectString,
        'weight': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_MoveContextCommand = de_MoveContextCommand;
const de_PauseExperimentCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'chosen_variant': smithy_client_1.expectString,
        'context': _ => de_Condition(_, context),
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'experiment_group_id': smithy_client_1.expectString,
        'experiment_type': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'metrics': _ => de_Document(_, context),
        'metrics_url': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'override_keys': smithy_client_1._json,
        'started_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'started_by': smithy_client_1.expectString,
        'status': smithy_client_1.expectString,
        'traffic_percentage': smithy_client_1.expectInt32,
        'variants': _ => de_ListVariant(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_PauseExperimentCommand = de_PauseExperimentCommand;
const de_PublishCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'draft_code': smithy_client_1.expectString,
        'draft_edited_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'draft_edited_by': smithy_client_1.expectString,
        'draft_runtime_version': smithy_client_1.expectString,
        'function_name': smithy_client_1.expectString,
        'function_type': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'published_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'published_by': smithy_client_1.expectString,
        'published_code': smithy_client_1.expectString,
        'published_runtime_version': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_PublishCommand = de_PublishCommand;
const de_RampExperimentCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'chosen_variant': smithy_client_1.expectString,
        'context': _ => de_Condition(_, context),
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'experiment_group_id': smithy_client_1.expectString,
        'experiment_type': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'metrics': _ => de_Document(_, context),
        'metrics_url': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'override_keys': smithy_client_1._json,
        'started_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'started_by': smithy_client_1.expectString,
        'status': smithy_client_1.expectString,
        'traffic_percentage': smithy_client_1.expectInt32,
        'variants': _ => de_ListVariant(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_RampExperimentCommand = de_RampExperimentCommand;
const de_RemoveMembersFromGroupCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'context': _ => de_Condition(_, context),
        'context_hash': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'member_experiment_ids': smithy_client_1._json,
        'name': smithy_client_1.expectString,
        'traffic_percentage': smithy_client_1.expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_RemoveMembersFromGroupCommand = de_RemoveMembersFromGroupCommand;
const de_ResumeExperimentCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'chosen_variant': smithy_client_1.expectString,
        'context': _ => de_Condition(_, context),
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'experiment_group_id': smithy_client_1.expectString,
        'experiment_type': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'metrics': _ => de_Document(_, context),
        'metrics_url': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'override_keys': smithy_client_1._json,
        'started_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'started_by': smithy_client_1.expectString,
        'status': smithy_client_1.expectString,
        'traffic_percentage': smithy_client_1.expectInt32,
        'variants': _ => de_ListVariant(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_ResumeExperimentCommand = de_ResumeExperimentCommand;
const de_TestCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'fn_output': _ => de_Document(_, context),
        'function_type': smithy_client_1.expectString,
        'stdout': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_TestCommand = de_TestCommand;
const de_UpdateDefaultConfigCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'autocomplete_function_name': smithy_client_1.expectString,
        'change_reason': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'function_name': smithy_client_1.expectString,
        'key': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'schema': _ => de_Document(_, context),
        'value': _ => de_Document(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_UpdateDefaultConfigCommand = de_UpdateDefaultConfigCommand;
const de_UpdateDimensionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'autocomplete_function_name': smithy_client_1.expectString,
        'change_reason': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'dependencies': smithy_client_1._json,
        'dependency_graph': _ => de_Object(_, context),
        'dependents': smithy_client_1._json,
        'description': smithy_client_1.expectString,
        'dimension': smithy_client_1.expectString,
        'function_name': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'mandatory': smithy_client_1.expectBoolean,
        'position': smithy_client_1.expectInt32,
        'schema': _ => de_Document(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_UpdateDimensionCommand = de_UpdateDimensionCommand;
const de_UpdateExperimentGroupCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'context': _ => de_Condition(_, context),
        'context_hash': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'member_experiment_ids': smithy_client_1._json,
        'name': smithy_client_1.expectString,
        'traffic_percentage': smithy_client_1.expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_UpdateExperimentGroupCommand = de_UpdateExperimentGroupCommand;
const de_UpdateFunctionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'draft_code': smithy_client_1.expectString,
        'draft_edited_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'draft_edited_by': smithy_client_1.expectString,
        'draft_runtime_version': smithy_client_1.expectString,
        'function_name': smithy_client_1.expectString,
        'function_type': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'published_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'published_by': smithy_client_1.expectString,
        'published_code': smithy_client_1.expectString,
        'published_runtime_version': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_UpdateFunctionCommand = de_UpdateFunctionCommand;
const de_UpdateOrganisationCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'admin_email': smithy_client_1.expectString,
        'contact_email': smithy_client_1.expectString,
        'contact_phone': smithy_client_1.expectString,
        'country_code': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'sector': smithy_client_1.expectString,
        'status': smithy_client_1.expectString,
        'updated_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'updated_by': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_UpdateOrganisationCommand = de_UpdateOrganisationCommand;
const de_UpdateOverrideCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'override': _ => de_Overrides(_, context),
        'override_id': smithy_client_1.expectString,
        'value': _ => de_Condition(_, context),
        'weight': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_UpdateOverrideCommand = de_UpdateOverrideCommand;
const de_UpdateOverridesExperimentCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'chosen_variant': smithy_client_1.expectString,
        'context': _ => de_Condition(_, context),
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'experiment_group_id': smithy_client_1.expectString,
        'experiment_type': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'metrics': _ => de_Document(_, context),
        'metrics_url': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'override_keys': smithy_client_1._json,
        'started_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'started_by': smithy_client_1.expectString,
        'status': smithy_client_1.expectString,
        'traffic_percentage': smithy_client_1.expectInt32,
        'variants': _ => de_ListVariant(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_UpdateOverridesExperimentCommand = de_UpdateOverridesExperimentCommand;
const de_UpdateTypeTemplatesCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'type_name': smithy_client_1.expectString,
        'type_schema': _ => de_Document(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_UpdateTypeTemplatesCommand = de_UpdateTypeTemplatesCommand;
const de_UpdateWebhookCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'change_reason': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'custom_headers': _ => de_Object(_, context),
        'description': smithy_client_1.expectString,
        'enabled': smithy_client_1.expectBoolean,
        'events': smithy_client_1._json,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'last_triggered_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'max_retries': smithy_client_1.expectInt32,
        'method': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'url': smithy_client_1.expectString,
        'version': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_UpdateWebhookCommand = de_UpdateWebhookCommand;
const de_UpdateWorkspaceCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'allow_experiment_self_approval': smithy_client_1.expectBoolean,
        'config_version': smithy_client_1.expectString,
        'created_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'last_modified_at': _ => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'mandatory_dimensions': smithy_client_1._json,
        'metrics': _ => de_Document(_, context),
        'organisation_id': smithy_client_1.expectString,
        'organisation_name': smithy_client_1.expectString,
        'strict_mode': smithy_client_1.expectBoolean,
        'workspace_admin_email': smithy_client_1.expectString,
        'workspace_name': smithy_client_1.expectString,
        'workspace_schema_name': smithy_client_1.expectString,
        'workspace_status': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_UpdateWorkspaceCommand = de_UpdateWorkspaceCommand;
const de_WeightRecomputeCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = (0, smithy_client_1.map)({
        $metadata: deserializeMetadata(output),
    });
    const data = (0, smithy_client_1.expectNonNull)(((0, smithy_client_1.expectObject)(await (0, core_1.parseJsonBody)(output.body, context))), "body");
    const doc = (0, smithy_client_1.take)(data, {
        'data': _ => de_WeightRecomputeResponses(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
exports.de_WeightRecomputeCommand = de_WeightRecomputeCommand;
const de_CommandError = async (output, context) => {
    const parsedOutput = {
        ...output,
        body: await (0, core_1.parseJsonErrorBody)(output.body, context)
    };
    const errorCode = (0, core_1.loadRestJsonErrorCode)(output, parsedOutput.body);
    switch (errorCode) {
        case "InternalServerError":
        case "io.superposition#InternalServerError":
            throw await de_InternalServerErrorRes(parsedOutput, context);
        case "ResourceNotFound":
        case "io.superposition#ResourceNotFound":
            throw await de_ResourceNotFoundRes(parsedOutput, context);
        case "FunctionNotFound":
        case "io.superposition#FunctionNotFound":
            throw await de_FunctionNotFoundRes(parsedOutput, context);
        case "TypeTemplatesNotFound":
        case "io.superposition#TypeTemplatesNotFound":
            throw await de_TypeTemplatesNotFoundRes(parsedOutput, context);
        case "OrganisationNotFound":
        case "io.superposition#OrganisationNotFound":
            throw await de_OrganisationNotFoundRes(parsedOutput, context);
        case "WebhookNotFound":
        case "io.superposition#WebhookNotFound":
            throw await de_WebhookNotFoundRes(parsedOutput, context);
        case "WorkspaceNotFound":
        case "io.superposition#WorkspaceNotFound":
            throw await de_WorkspaceNotFoundRes(parsedOutput, context);
        default:
            const parsedBody = parsedOutput.body;
            return throwDefaultError({
                output,
                parsedBody,
                errorCode
            });
    }
};
const throwDefaultError = (0, smithy_client_1.withBaseException)(SuperpositionServiceException_1.SuperpositionServiceException);
const de_FunctionNotFoundRes = async (parsedOutput, context) => {
    const contents = (0, smithy_client_1.map)({});
    const data = parsedOutput.body;
    const doc = (0, smithy_client_1.take)(data, {});
    Object.assign(contents, doc);
    const exception = new models_0_1.FunctionNotFound({
        $metadata: deserializeMetadata(parsedOutput),
        ...contents
    });
    return (0, smithy_client_1.decorateServiceException)(exception, parsedOutput.body);
};
const de_InternalServerErrorRes = async (parsedOutput, context) => {
    const contents = (0, smithy_client_1.map)({});
    const data = parsedOutput.body;
    const doc = (0, smithy_client_1.take)(data, {
        'message': smithy_client_1.expectString,
    });
    Object.assign(contents, doc);
    const exception = new models_0_1.InternalServerError({
        $metadata: deserializeMetadata(parsedOutput),
        ...contents
    });
    return (0, smithy_client_1.decorateServiceException)(exception, parsedOutput.body);
};
const de_OrganisationNotFoundRes = async (parsedOutput, context) => {
    const contents = (0, smithy_client_1.map)({});
    const data = parsedOutput.body;
    const doc = (0, smithy_client_1.take)(data, {});
    Object.assign(contents, doc);
    const exception = new models_0_1.OrganisationNotFound({
        $metadata: deserializeMetadata(parsedOutput),
        ...contents
    });
    return (0, smithy_client_1.decorateServiceException)(exception, parsedOutput.body);
};
const de_ResourceNotFoundRes = async (parsedOutput, context) => {
    const contents = (0, smithy_client_1.map)({});
    const data = parsedOutput.body;
    const doc = (0, smithy_client_1.take)(data, {});
    Object.assign(contents, doc);
    const exception = new models_0_1.ResourceNotFound({
        $metadata: deserializeMetadata(parsedOutput),
        ...contents
    });
    return (0, smithy_client_1.decorateServiceException)(exception, parsedOutput.body);
};
const de_TypeTemplatesNotFoundRes = async (parsedOutput, context) => {
    const contents = (0, smithy_client_1.map)({});
    const data = parsedOutput.body;
    const doc = (0, smithy_client_1.take)(data, {});
    Object.assign(contents, doc);
    const exception = new models_0_1.TypeTemplatesNotFound({
        $metadata: deserializeMetadata(parsedOutput),
        ...contents
    });
    return (0, smithy_client_1.decorateServiceException)(exception, parsedOutput.body);
};
const de_WebhookNotFoundRes = async (parsedOutput, context) => {
    const contents = (0, smithy_client_1.map)({});
    const data = parsedOutput.body;
    const doc = (0, smithy_client_1.take)(data, {});
    Object.assign(contents, doc);
    const exception = new models_0_1.WebhookNotFound({
        $metadata: deserializeMetadata(parsedOutput),
        ...contents
    });
    return (0, smithy_client_1.decorateServiceException)(exception, parsedOutput.body);
};
const de_WorkspaceNotFoundRes = async (parsedOutput, context) => {
    const contents = (0, smithy_client_1.map)({});
    const data = parsedOutput.body;
    const doc = (0, smithy_client_1.take)(data, {});
    Object.assign(contents, doc);
    const exception = new models_0_1.WorkspaceNotFound({
        $metadata: deserializeMetadata(parsedOutput),
        ...contents
    });
    return (0, smithy_client_1.decorateServiceException)(exception, parsedOutput.body);
};
const se_AutocompleteFunctionRequest = (input, context) => {
    return (0, smithy_client_1.take)(input, {
        'environment': _ => se_Document(_, context),
        'name': [],
        'prefix': [],
    });
};
const se_BulkOperationList = (input, context) => {
    return input.filter((e) => e != null).map(entry => {
        return se_ContextAction(entry, context);
    });
};
const se_BulkOperationReq = (input, context) => {
    return (0, smithy_client_1.take)(input, {
        'operations': _ => se_BulkOperationList(_, context),
    });
};
const se_Condition = (input, context) => {
    return Object.entries(input).reduce((acc, [key, value]) => {
        if (value === null) {
            return acc;
        }
        acc[key] = se_Document(value, context);
        return acc;
    }, {});
};
const se_ContextAction = (input, context) => {
    return models_0_1.ContextAction.visit(input, {
        DELETE: value => ({ "DELETE": value }),
        MOVE: value => ({ "MOVE": se_ContextMove(value, context) }),
        PUT: value => ({ "PUT": se_ContextPut(value, context) }),
        REPLACE: value => ({ "REPLACE": se_UpdateContextOverrideRequest(value, context) }),
        _: (name, value) => ({ name: value })
    });
};
const se_ContextIdentifier = (input, context) => {
    return models_0_1.ContextIdentifier.visit(input, {
        context: value => ({ "context": se_Condition(value, context) }),
        id: value => ({ "id": value }),
        _: (name, value) => ({ name: value })
    });
};
const se_ContextMap = (input, context) => {
    return Object.entries(input).reduce((acc, [key, value]) => {
        if (value === null) {
            return acc;
        }
        acc[key] = se_Document(value, context);
        return acc;
    }, {});
};
const se_ContextMove = (input, context) => {
    return (0, smithy_client_1.take)(input, {
        'change_reason': [],
        'context': _ => se_Condition(_, context),
        'description': [],
        'id': [],
    });
};
const se_ContextPut = (input, context) => {
    return (0, smithy_client_1.take)(input, {
        'change_reason': [],
        'context': _ => se_Condition(_, context),
        'description': [],
        'override': _ => se_Overrides(_, context),
    });
};
const se_FunctionExecutionRequest = (input, context) => {
    return models_0_1.FunctionExecutionRequest.visit(input, {
        AutocompleteFunctionRequest: value => ({ "AutocompleteFunctionRequest": se_AutocompleteFunctionRequest(value, context) }),
        ValidateFunctionRequest: value => ({ "ValidateFunctionRequest": se_ValidateFunctionRequest(value, context) }),
        _: (name, value) => ({ name: value })
    });
};
const se_ListVariant = (input, context) => {
    return input.filter((e) => e != null).map(entry => {
        return se_Variant(entry, context);
    });
};
const se_ListVariantUpdateRequest = (input, context) => {
    return input.filter((e) => e != null).map(entry => {
        return se_VariantUpdateRequest(entry, context);
    });
};
const se_Object = (input, context) => {
    return Object.entries(input).reduce((acc, [key, value]) => {
        if (value === null) {
            return acc;
        }
        acc[key] = se_Document(value, context);
        return acc;
    }, {});
};
const se_Overrides = (input, context) => {
    return Object.entries(input).reduce((acc, [key, value]) => {
        if (value === null) {
            return acc;
        }
        acc[key] = se_Document(value, context);
        return acc;
    }, {});
};
const se_UpdateContextOverrideRequest = (input, context) => {
    return (0, smithy_client_1.take)(input, {
        'change_reason': [],
        'context': _ => se_ContextIdentifier(_, context),
        'description': [],
        'override': _ => se_Overrides(_, context),
    });
};
const se_ValidateFunctionRequest = (input, context) => {
    return (0, smithy_client_1.take)(input, {
        'key': [],
        'value': _ => se_Document(_, context),
    });
};
const se_Variant = (input, context) => {
    return (0, smithy_client_1.take)(input, {
        'context_id': [],
        'id': [],
        'override_id': [],
        'overrides': _ => se_Document(_, context),
        'variant_type': [],
    });
};
const se_VariantUpdateRequest = (input, context) => {
    return (0, smithy_client_1.take)(input, {
        'id': [],
        'overrides': _ => se_Document(_, context),
    });
};
const se_Document = (input, context) => {
    return input;
};
const de_AuditLogFull = (output, context) => {
    return (0, smithy_client_1.take)(output, {
        'action': smithy_client_1.expectString,
        'new_data': (_) => de_Document(_, context),
        'original_data': (_) => de_Document(_, context),
        'query': smithy_client_1.expectString,
        'table_name': smithy_client_1.expectString,
        'timestamp': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'user_name': smithy_client_1.expectString,
    });
};
const de_AuditLogList = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_AuditLogFull(entry, context);
    });
    return retVal;
};
const de_Condition = (output, context) => {
    return Object.entries(output).reduce((acc, [key, value]) => {
        if (value === null) {
            return acc;
        }
        acc[key] = de_Document(value, context);
        return acc;
    }, {});
};
const de_ContextFull = (output, context) => {
    return (0, smithy_client_1.take)(output, {
        'change_reason': smithy_client_1.expectString,
        'created_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'override': (_) => de_Overrides(_, context),
        'override_id': smithy_client_1.expectString,
        'value': (_) => de_Condition(_, context),
        'weight': smithy_client_1.expectString,
    });
};
const de_ContextList = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_ContextPartial(entry, context);
    });
    return retVal;
};
const de_ContextPartial = (output, context) => {
    return (0, smithy_client_1.take)(output, {
        'condition': (_) => de_Condition(_, context),
        'id': smithy_client_1.expectString,
        'override_with_keys': smithy_client_1._json,
        'priority': smithy_client_1.expectInt32,
        'weight': smithy_client_1.expectInt32,
    });
};
const de_DefaultConfigFull = (output, context) => {
    return (0, smithy_client_1.take)(output, {
        'autocomplete_function_name': smithy_client_1.expectString,
        'change_reason': smithy_client_1.expectString,
        'created_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'function_name': smithy_client_1.expectString,
        'key': smithy_client_1.expectString,
        'last_modified_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'schema': (_) => de_Document(_, context),
        'value': (_) => de_Document(_, context),
    });
};
const de_DimensionExt = (output, context) => {
    return (0, smithy_client_1.take)(output, {
        'autocomplete_function_name': smithy_client_1.expectString,
        'change_reason': smithy_client_1.expectString,
        'created_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'dependencies': smithy_client_1._json,
        'dependency_graph': (_) => de_Object(_, context),
        'dependents': smithy_client_1._json,
        'description': smithy_client_1.expectString,
        'dimension': smithy_client_1.expectString,
        'function_name': smithy_client_1.expectString,
        'last_modified_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'mandatory': smithy_client_1.expectBoolean,
        'position': smithy_client_1.expectInt32,
        'schema': (_) => de_Document(_, context),
    });
};
const de_DimensionExtList = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_DimensionExt(entry, context);
    });
    return retVal;
};
const de_ExperimentGroupList = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_ExperimentGroupResponse(entry, context);
    });
    return retVal;
};
const de_ExperimentGroupResponse = (output, context) => {
    return (0, smithy_client_1.take)(output, {
        'change_reason': smithy_client_1.expectString,
        'context': (_) => de_Condition(_, context),
        'context_hash': smithy_client_1.expectString,
        'created_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'member_experiment_ids': smithy_client_1._json,
        'name': smithy_client_1.expectString,
        'traffic_percentage': smithy_client_1.expectInt32,
    });
};
const de_ExperimentList = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_ExperimentResponse(entry, context);
    });
    return retVal;
};
const de_ExperimentResponse = (output, context) => {
    return (0, smithy_client_1.take)(output, {
        'change_reason': smithy_client_1.expectString,
        'chosen_variant': smithy_client_1.expectString,
        'context': (_) => de_Condition(_, context),
        'created_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'experiment_group_id': smithy_client_1.expectString,
        'experiment_type': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'last_modified': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'metrics': (_) => de_Document(_, context),
        'metrics_url': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'override_keys': smithy_client_1._json,
        'started_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'started_by': smithy_client_1.expectString,
        'status': smithy_client_1.expectString,
        'traffic_percentage': smithy_client_1.expectInt32,
        'variants': (_) => de_ListVariant(_, context),
    });
};
const de_FunctionListResponse = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_FunctionResponse(entry, context);
    });
    return retVal;
};
const de_FunctionResponse = (output, context) => {
    return (0, smithy_client_1.take)(output, {
        'change_reason': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'draft_code': smithy_client_1.expectString,
        'draft_edited_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'draft_edited_by': smithy_client_1.expectString,
        'draft_runtime_version': smithy_client_1.expectString,
        'function_name': smithy_client_1.expectString,
        'function_type': smithy_client_1.expectString,
        'last_modified_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'published_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'published_by': smithy_client_1.expectString,
        'published_code': smithy_client_1.expectString,
        'published_runtime_version': smithy_client_1.expectString,
    });
};
const de_ListContextOut = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_ContextFull(entry, context);
    });
    return retVal;
};
const de_ListDefaultConfigOut = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_DefaultConfigFull(entry, context);
    });
    return retVal;
};
const de_ListVariant = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_Variant(entry, context);
    });
    return retVal;
};
const de_ListVersionsMember = (output, context) => {
    return (0, smithy_client_1.take)(output, {
        'config': (_) => de_Document(_, context),
        'config_hash': smithy_client_1.expectString,
        'created_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'description': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'tags': smithy_client_1._json,
    });
};
const de_ListVersionsOut = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_ListVersionsMember(entry, context);
    });
    return retVal;
};
const de_Object = (output, context) => {
    return Object.entries(output).reduce((acc, [key, value]) => {
        if (value === null) {
            return acc;
        }
        acc[key] = de_Document(value, context);
        return acc;
    }, {});
};
const de_OrganisationList = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_OrganisationResponse(entry, context);
    });
    return retVal;
};
const de_OrganisationResponse = (output, context) => {
    return (0, smithy_client_1.take)(output, {
        'admin_email': smithy_client_1.expectString,
        'contact_email': smithy_client_1.expectString,
        'contact_phone': smithy_client_1.expectString,
        'country_code': smithy_client_1.expectString,
        'created_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'sector': smithy_client_1.expectString,
        'status': smithy_client_1.expectString,
        'updated_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'updated_by': smithy_client_1.expectString,
    });
};
const de_Overrides = (output, context) => {
    return Object.entries(output).reduce((acc, [key, value]) => {
        if (value === null) {
            return acc;
        }
        acc[key] = de_Document(value, context);
        return acc;
    }, {});
};
const de_OverridesMap = (output, context) => {
    return Object.entries(output).reduce((acc, [key, value]) => {
        if (value === null) {
            return acc;
        }
        acc[key] = de_Overrides(value, context);
        return acc;
    }, {});
};
const de_TypeTemplatesList = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_TypeTemplatesResponse(entry, context);
    });
    return retVal;
};
const de_TypeTemplatesResponse = (output, context) => {
    return (0, smithy_client_1.take)(output, {
        'change_reason': smithy_client_1.expectString,
        'created_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'description': smithy_client_1.expectString,
        'last_modified_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'type_name': smithy_client_1.expectString,
        'type_schema': (_) => de_Document(_, context),
    });
};
const de_Variant = (output, context) => {
    return (0, smithy_client_1.take)(output, {
        'context_id': smithy_client_1.expectString,
        'id': smithy_client_1.expectString,
        'override_id': smithy_client_1.expectString,
        'overrides': (_) => de_Document(_, context),
        'variant_type': smithy_client_1.expectString,
    });
};
const de_WebhookList = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_WebhookResponse(entry, context);
    });
    return retVal;
};
const de_WebhookResponse = (output, context) => {
    return (0, smithy_client_1.take)(output, {
        'change_reason': smithy_client_1.expectString,
        'created_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'custom_headers': (_) => de_Object(_, context),
        'description': smithy_client_1.expectString,
        'enabled': smithy_client_1.expectBoolean,
        'events': smithy_client_1._json,
        'last_modified_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'last_triggered_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'max_retries': smithy_client_1.expectInt32,
        'method': smithy_client_1.expectString,
        'name': smithy_client_1.expectString,
        'url': smithy_client_1.expectString,
        'version': smithy_client_1.expectString,
    });
};
const de_WeightRecomputeResponse = (output, context) => {
    return (0, smithy_client_1.take)(output, {
        'condition': (_) => de_Condition(_, context),
        'id': smithy_client_1.expectString,
        'new_weight': smithy_client_1.expectString,
        'old_weight': smithy_client_1.expectString,
    });
};
const de_WeightRecomputeResponses = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_WeightRecomputeResponse(entry, context);
    });
    return retVal;
};
const de_WorkspaceList = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_WorkspaceResponse(entry, context);
    });
    return retVal;
};
const de_WorkspaceResponse = (output, context) => {
    return (0, smithy_client_1.take)(output, {
        'allow_experiment_self_approval': smithy_client_1.expectBoolean,
        'config_version': smithy_client_1.expectString,
        'created_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'created_by': smithy_client_1.expectString,
        'last_modified_at': (_) => (0, smithy_client_1.expectNonNull)((0, smithy_client_1.parseRfc3339DateTimeWithOffset)(_)),
        'last_modified_by': smithy_client_1.expectString,
        'mandatory_dimensions': smithy_client_1._json,
        'metrics': (_) => de_Document(_, context),
        'organisation_id': smithy_client_1.expectString,
        'organisation_name': smithy_client_1.expectString,
        'strict_mode': smithy_client_1.expectBoolean,
        'workspace_admin_email': smithy_client_1.expectString,
        'workspace_name': smithy_client_1.expectString,
        'workspace_schema_name': smithy_client_1.expectString,
        'workspace_status': smithy_client_1.expectString,
    });
};
const de_Document = (output, context) => {
    return output;
};
const deserializeMetadata = (output) => ({
    httpStatusCode: output.statusCode,
    requestId: output.headers["x-amzn-requestid"] ?? output.headers["x-amzn-request-id"] ?? output.headers["x-amz-request-id"],
    extendedRequestId: output.headers["x-amz-id-2"],
    cfId: output.headers["x-amz-cf-id"],
});
const collectBodyString = (streamBody, context) => (0, smithy_client_1.collectBody)(streamBody, context).then(body => context.utf8Encoder(body));
const _a = "all";
const _ac = "action";
const _ai = "audit_id";
const _c = "count";
const _cb = "created_by";
const _ci = "context_id";
const _ct = "config_tags";
const _egi = "experiment_group_ids";
const _ei = "experiment_ids";
const _en = "experiment_name";
const _fd = "from_date";
const _lm = "last-modified";
const _lm_ = "last_modified";
const _lmb = "last_modified_by";
const _ms = "merge_strategy";
const _n = "name";
const _oi = "org_id";
const _p = "prefix";
const _pa = "page";
const _pl = "plaintext";
const _s = "status";
const _sb = "sort_by";
const _so = "sort_on";
const _sr = "show_reasoning";
const _t = "tables";
const _ta = "table";
const _td = "to_date";
const _u = "username";
const _v = "version";
const _wi = "workspace_id";
const _xai = "x-audit-id";
const _xct = "x-config-tags";
const _xcv = "x-config-version";
const _xms = "x-merge-strategy";
const _xoi = "x-org-id";
const _xt = "x-tenant";
