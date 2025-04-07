import { SuperpositionServiceException as __BaseException } from "../models/SuperpositionServiceException";
import { ContextAction, FunctionExecutionRequest, FunctionNotFound, InternalServerError, OrganisationNotFound, ResourceNotFound, TypeTemplatesNotFound, WorkspaceNotFound, } from "../models/models_0";
import { loadRestJsonErrorCode, parseJsonBody as parseBody, parseJsonErrorBody as parseErrorBody, } from "@aws-sdk/core";
import { requestBuilder as rb } from "@smithy/core";
import { decorateServiceException as __decorateServiceException, expectBoolean as __expectBoolean, expectInt32 as __expectInt32, expectLong as __expectLong, expectNonNull as __expectNonNull, expectObject as __expectObject, expectString as __expectString, parseRfc3339DateTimeWithOffset as __parseRfc3339DateTimeWithOffset, serializeDateTime as __serializeDateTime, _json, collectBody, isSerializableHeaderValue, map, take, withBaseException, } from "@smithy/smithy-client";
export const se_ApplicableVariantsCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments/applicable-variants");
    let body;
    body = JSON.stringify(take(input, {
        'context': _ => se_Condition(_, context),
        'toss': [],
    }));
    b.m("POST")
        .h(headers)
        .b(body);
    return b.build();
};
export const se_BulkOperationCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
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
export const se_ConcludeExperimentCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments/{id}/conclude");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify(take(input, {
        'change_reason': [],
        'chosen_variant': [],
        'description': [],
    }));
    b.m("PATCH")
        .h(headers)
        .b(body);
    return b.build();
};
export const se_CreateContextCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
        [_xct]: input[_ct],
    });
    b.bp("/context");
    let body;
    body = JSON.stringify(take(input, {
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
export const se_CreateDefaultConfigCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/default-config");
    let body;
    body = JSON.stringify(take(input, {
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
export const se_CreateDimensionCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/dimension");
    let body;
    body = JSON.stringify(take(input, {
        'change_reason': [],
        'dependencies': _ => _json(_),
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
export const se_CreateExperimentCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments");
    let body;
    body = JSON.stringify(take(input, {
        'change_reason': [],
        'context': _ => se_Condition(_, context),
        'description': [],
        'name': [],
        'variants': _ => se_ListVariant(_, context),
    }));
    b.m("POST")
        .h(headers)
        .b(body);
    return b.build();
};
export const se_CreateFunctionCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/function");
    let body;
    body = JSON.stringify(take(input, {
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
export const se_CreateOrganisationCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = {
        'content-type': 'application/json',
    };
    b.bp("/superposition/organisations");
    let body;
    body = JSON.stringify(take(input, {
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
export const se_CreateTypeTemplatesCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/types");
    let body;
    body = JSON.stringify(take(input, {
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
export const se_CreateWorkspaceCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xoi]: input[_oi],
    });
    b.bp("/workspaces");
    let body;
    body = JSON.stringify(take(input, {
        'workspace_admin_email': [],
        'workspace_name': [],
        'workspace_status': [],
        'workspace_strict_mode': [],
    }));
    b.m("POST")
        .h(headers)
        .b(body);
    return b.build();
};
export const se_DeleteContextCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
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
export const se_DeleteDefaultConfigCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
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
export const se_DeleteDimensionCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
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
export const se_DeleteFunctionCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
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
export const se_DeleteTypeTemplatesCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
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
export const se_DiscardExperimentCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments/{id}/discard");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify(take(input, {
        'change_reason': [],
    }));
    b.m("PATCH")
        .h(headers)
        .b(body);
    return b.build();
};
export const se_GetConfigCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/config");
    const query = map({
        [_p]: [, input[_p]],
        [_v]: [, input[_v]],
    });
    let body;
    body = JSON.stringify(take(input, {
        'context': _ => se_ContextMap(_, context),
    }));
    b.m("POST")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
export const se_GetConfigFastCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
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
export const se_GetContextCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
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
export const se_GetContextFromConditionCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
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
export const se_GetExperimentCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
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
export const se_GetFunctionCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
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
export const se_GetOrganisationCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = {};
    b.bp("/superposition/organisations/{id}");
    b.p('id', () => input.id, '{id}', false);
    let body;
    b.m("GET")
        .h(headers)
        .b(body);
    return b.build();
};
export const se_GetResolvedConfigCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/config/resolve");
    const query = map({
        [_p]: [, input[_p]],
        [_v]: [, input[_v]],
        [_sr]: [() => input.show_reasoning !== void 0, () => (input[_sr].toString())],
        [_xms]: [, input[_ms]],
    });
    let body;
    body = JSON.stringify(take(input, {
        'context': _ => se_ContextMap(_, context),
    }));
    b.m("POST")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
export const se_GetTypeTemplatesListCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/types");
    const query = map({
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
export const se_ListAuditLogsCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/audit");
    const query = map({
        [_c]: [() => input.count !== void 0, () => (input[_c].toString())],
        [_pa]: [() => input.page !== void 0, () => (input[_pa].toString())],
        [_fd]: [() => input.from_date !== void 0, () => (__serializeDateTime(input[_fd]).toString())],
        [_td]: [() => input.to_date !== void 0, () => (__serializeDateTime(input[_td]).toString())],
        [_ta]: [, input[_t]],
        [_a]: [, input[_a]],
        [_u]: [, input[_u]],
    });
    let body;
    b.m("GET")
        .h(headers)
        .q(query)
        .b(body);
    return b.build();
};
export const se_ListContextsCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/context/list");
    const query = map({
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
export const se_ListDefaultConfigsCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/default-config");
    const query = map({
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
export const se_ListDimensionsCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/dimension");
    const query = map({
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
export const se_ListExperimentCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments");
    const query = map({
        [_pa]: [() => input.page !== void 0, () => (input[_pa].toString())],
        [_c]: [() => input.count !== void 0, () => (input[_c].toString())],
        [_al]: [() => input.all !== void 0, () => (input[_al].toString())],
        [_s]: [, input[_s]],
        [_fd]: [() => input.from_date !== void 0, () => (__serializeDateTime(input[_fd]).toString())],
        [_td]: [() => input.to_date !== void 0, () => (__serializeDateTime(input[_td]).toString())],
        [_en]: [, input[_en]],
        [_ei]: [, input[_ei]],
        [_cb]: [, input[_cb]],
        [_co]: [, input[_cq]],
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
export const se_ListFunctionCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/function");
    const query = map({
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
export const se_ListOrganisationCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = {};
    b.bp("/superposition/organisations");
    const query = map({
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
export const se_ListVersionsCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/config/versions");
    const query = map({
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
export const se_ListWorkspaceCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        [_xoi]: input[_oi],
    });
    b.bp("/workspaces");
    const query = map({
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
export const se_MoveContextCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/context/move/{id}");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify(take(input, {
        'change_reason': [],
        'context': _ => se_Condition(_, context),
        'description': [],
    }));
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
export const se_PublishCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/function/{function_name}/publish");
    b.p('function_name', () => input.function_name, '{function_name}', false);
    let body;
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
export const se_RampExperimentCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments/{id}/ramp");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify(take(input, {
        'change_reason': [],
        'traffic_percentage': [],
    }));
    b.m("PATCH")
        .h(headers)
        .b(body);
    return b.build();
};
export const se_TestCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
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
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
export const se_UpdateDefaultConfigCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/default-config/{key}");
    b.p('key', () => input.key, '{key}', false);
    let body;
    body = JSON.stringify(take(input, {
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
export const se_UpdateDimensionCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/dimension/{dimension}");
    b.p('dimension', () => input.dimension, '{dimension}', false);
    let body;
    body = JSON.stringify(take(input, {
        'change_reason': [],
        'dependencies': _ => _json(_),
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
export const se_UpdateFunctionCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/function/{function_name}");
    b.p('function_name', () => input.function_name, '{function_name}', false);
    let body;
    body = JSON.stringify(take(input, {
        'change_reason': [],
        'description': [],
        'function': [],
        'function_type': [],
        'runtime_version': [],
    }));
    b.m("PATCH")
        .h(headers)
        .b(body);
    return b.build();
};
export const se_UpdateOrganisationCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = {
        'content-type': 'application/json',
    };
    b.bp("/superposition/organisations/{id}");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify(take(input, {
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
export const se_UpdateOverrideCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
        [_xct]: input[_ct],
    });
    b.bp("/context/overrides");
    let body;
    body = JSON.stringify(take(input, {
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
export const se_UpdateOverridesExperimentCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/experiments/{id}/overrides");
    b.p('id', () => input.id, '{id}', false);
    let body;
    body = JSON.stringify(take(input, {
        'change_reason': [],
        'description': [],
        'variant_list': _ => se_ListVariantUpdateRequest(_, context),
    }));
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
export const se_UpdateTypeTemplatesCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xt]: input[_wi],
        [_xoi]: input[_oi],
    });
    b.bp("/types/{type_name}");
    b.p('type_name', () => input.type_name, '{type_name}', false);
    let body;
    body = JSON.stringify(take(input, {
        'change_reason': [],
        'description': [],
        'type_schema': _ => se_Document(_, context),
    }));
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
export const se_UpdateWorkspaceCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
        'content-type': 'application/json',
        [_xoi]: input[_oi],
    });
    b.bp("/workspaces/{workspace_name}");
    b.p('workspace_name', () => input.workspace_name, '{workspace_name}', false);
    let body;
    body = JSON.stringify(take(input, {
        'mandatory_dimensions': _ => _json(_),
        'workspace_admin_email': [],
        'workspace_status': [],
    }));
    b.m("PUT")
        .h(headers)
        .b(body);
    return b.build();
};
export const se_WeightRecomputeCommand = async (input, context) => {
    const b = rb(input, context);
    const headers = map({}, isSerializableHeaderValue, {
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
export const de_ApplicableVariantsCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'data': _ => de_ListVariant(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_BulkOperationCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectObject(await parseBody(output.body, context));
    contents.bulk_operation_output = _json(data);
    return contents;
};
export const de_ConcludeExperimentCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'chosen_variant': __expectString,
        'context': _ => de_Condition(_, context),
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'id': __expectString,
        'last_modified': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'name': __expectString,
        'override_keys': _json,
        'status': __expectString,
        'traffic_percentage': __expectInt32,
        'variants': _ => de_ListVariant(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_CreateContextCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'context_id': __expectString,
        'description': __expectString,
        'override_id': __expectString,
        'weight': __expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_CreateDefaultConfigCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'function_name': __expectString,
        'key': __expectString,
        'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'schema': _ => de_Document(_, context),
        'value': _ => de_Document(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_CreateDimensionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'dependencies': _json,
        'dependency_graph': _ => de_Object(_, context),
        'dependents': _json,
        'description': __expectString,
        'dimension': __expectString,
        'function_name': __expectString,
        'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'mandatory': __expectBoolean,
        'position': __expectInt32,
        'schema': _ => de_Document(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_CreateExperimentCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'chosen_variant': __expectString,
        'context': _ => de_Condition(_, context),
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'id': __expectString,
        'last_modified': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'name': __expectString,
        'override_keys': _json,
        'status': __expectString,
        'traffic_percentage': __expectInt32,
        'variants': _ => de_ListVariant(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_CreateFunctionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'description': __expectString,
        'draft_code': __expectString,
        'draft_edited_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'draft_edited_by': __expectString,
        'draft_runtime_version': __expectString,
        'function_name': __expectString,
        'function_type': __expectString,
        'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'published_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'published_by': __expectString,
        'published_code': __expectString,
        'published_runtime_version': __expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_CreateOrganisationCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'admin_email': __expectString,
        'contact_email': __expectString,
        'contact_phone': __expectString,
        'country_code': __expectString,
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'id': __expectString,
        'name': __expectString,
        'sector': __expectString,
        'status': __expectString,
        'updated_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'updated_by': __expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_CreateTypeTemplatesCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'type_name': __expectString,
        'type_schema': _ => de_Document(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_CreateWorkspaceCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'mandatory_dimensions': _json,
        'organisation_id': __expectString,
        'organisation_name': __expectString,
        'workspace_admin_email': __expectString,
        'workspace_name': __expectString,
        'workspace_schema_name': __expectString,
        'workspace_status': __expectString,
        'workspace_strict_mode': __expectBoolean,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_DeleteContextCommand = async (output, context) => {
    if (output.statusCode !== 201 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    await collectBody(output.body, context);
    return contents;
};
export const de_DeleteDefaultConfigCommand = async (output, context) => {
    if (output.statusCode !== 201 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    await collectBody(output.body, context);
    return contents;
};
export const de_DeleteDimensionCommand = async (output, context) => {
    if (output.statusCode !== 201 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    await collectBody(output.body, context);
    return contents;
};
export const de_DeleteFunctionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    await collectBody(output.body, context);
    return contents;
};
export const de_DeleteTypeTemplatesCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'type_name': __expectString,
        'type_schema': _ => de_Document(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_DiscardExperimentCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'chosen_variant': __expectString,
        'context': _ => de_Condition(_, context),
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'id': __expectString,
        'last_modified': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'name': __expectString,
        'override_keys': _json,
        'status': __expectString,
        'traffic_percentage': __expectInt32,
        'variants': _ => de_ListVariant(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_GetConfigCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
        [_v]: [, output.headers[_xcv]],
        [_lm_]: [() => void 0 !== output.headers[_lm], () => __expectNonNull(__parseRfc3339DateTimeWithOffset(output.headers[_lm]))],
        [_ai]: [, output.headers[_xai]],
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'contexts': _ => de_ContextList(_, context),
        'default_configs': _ => de_Object(_, context),
        'overrides': _ => de_OverridesMap(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_GetConfigFastCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
        [_v]: [, output.headers[_xcv]],
        [_lm_]: [() => void 0 !== output.headers[_lm], () => __expectNonNull(__parseRfc3339DateTimeWithOffset(output.headers[_lm]))],
        [_ai]: [, output.headers[_xai]],
    });
    const data = await collectBodyString(output.body, context);
    contents.config = data;
    contents.config = JSON.parse(data);
    return contents;
};
export const de_GetContextCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'id': __expectString,
        'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'override': _ => de_Overrides(_, context),
        'override_id': __expectString,
        'value': _ => de_Condition(_, context),
        'weight': __expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_GetContextFromConditionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'id': __expectString,
        'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'override': _ => de_Overrides(_, context),
        'override_id': __expectString,
        'value': _ => de_Condition(_, context),
        'weight': __expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_GetExperimentCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'chosen_variant': __expectString,
        'context': _ => de_Condition(_, context),
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'id': __expectString,
        'last_modified': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'name': __expectString,
        'override_keys': _json,
        'status': __expectString,
        'traffic_percentage': __expectInt32,
        'variants': _ => de_ListVariant(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_GetFunctionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'description': __expectString,
        'draft_code': __expectString,
        'draft_edited_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'draft_edited_by': __expectString,
        'draft_runtime_version': __expectString,
        'function_name': __expectString,
        'function_type': __expectString,
        'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'published_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'published_by': __expectString,
        'published_code': __expectString,
        'published_runtime_version': __expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_GetOrganisationCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'admin_email': __expectString,
        'contact_email': __expectString,
        'contact_phone': __expectString,
        'country_code': __expectString,
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'id': __expectString,
        'name': __expectString,
        'sector': __expectString,
        'status': __expectString,
        'updated_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'updated_by': __expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_GetResolvedConfigCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
        [_v]: [, output.headers[_xcv]],
        [_lm_]: [() => void 0 !== output.headers[_lm], () => __expectNonNull(__parseRfc3339DateTimeWithOffset(output.headers[_lm]))],
        [_ai]: [, output.headers[_xai]],
    });
    const data = await collectBodyString(output.body, context);
    contents.config = data;
    contents.config = JSON.parse(data);
    return contents;
};
export const de_GetTypeTemplatesListCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'data': _ => de_TypeTemplatesList(_, context),
        'total_items': __expectInt32,
        'total_pages': __expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_ListAuditLogsCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'data': _ => de_AuditLogList(_, context),
        'total_items': __expectInt32,
        'total_pages': __expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_ListContextsCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'data': _ => de_ListContextOut(_, context),
        'total_items': __expectInt32,
        'total_pages': __expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_ListDefaultConfigsCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'data': _ => de_ListDefaultConfigOut(_, context),
        'total_items': __expectInt32,
        'total_pages': __expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_ListDimensionsCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'data': _ => de_DimensionExtList(_, context),
        'total_items': __expectInt32,
        'total_pages': __expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_ListExperimentCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'data': _ => de_ExperimentList(_, context),
        'total_items': __expectLong,
        'total_pages': __expectLong,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_ListFunctionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'data': _ => de_FunctionListResponse(_, context),
        'total_items': __expectInt32,
        'total_pages': __expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_ListOrganisationCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'data': _ => de_OrganisationList(_, context),
        'total_items': __expectInt32,
        'total_pages': __expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_ListVersionsCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'data': _ => de_ListVersionsOut(_, context),
        'total_items': __expectInt32,
        'total_pages': __expectInt32,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_ListWorkspaceCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'data': _ => de_WorkspaceList(_, context),
        'total_items': __expectLong,
        'total_pages': __expectLong,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_MoveContextCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'context_id': __expectString,
        'description': __expectString,
        'override_id': __expectString,
        'weight': __expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_PublishCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'description': __expectString,
        'draft_code': __expectString,
        'draft_edited_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'draft_edited_by': __expectString,
        'draft_runtime_version': __expectString,
        'function_name': __expectString,
        'function_type': __expectString,
        'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'published_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'published_by': __expectString,
        'published_code': __expectString,
        'published_runtime_version': __expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_RampExperimentCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'chosen_variant': __expectString,
        'context': _ => de_Condition(_, context),
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'id': __expectString,
        'last_modified': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'name': __expectString,
        'override_keys': _json,
        'status': __expectString,
        'traffic_percentage': __expectInt32,
        'variants': _ => de_ListVariant(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_TestCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'fn_output': _ => de_Document(_, context),
        'function_type': __expectString,
        'stdout': __expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_UpdateDefaultConfigCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'function_name': __expectString,
        'key': __expectString,
        'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'schema': _ => de_Document(_, context),
        'value': _ => de_Document(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_UpdateDimensionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'dependencies': _json,
        'dependency_graph': _ => de_Object(_, context),
        'dependents': _json,
        'description': __expectString,
        'dimension': __expectString,
        'function_name': __expectString,
        'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'mandatory': __expectBoolean,
        'position': __expectInt32,
        'schema': _ => de_Document(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_UpdateFunctionCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'description': __expectString,
        'draft_code': __expectString,
        'draft_edited_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'draft_edited_by': __expectString,
        'draft_runtime_version': __expectString,
        'function_name': __expectString,
        'function_type': __expectString,
        'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'published_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'published_by': __expectString,
        'published_code': __expectString,
        'published_runtime_version': __expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_UpdateOrganisationCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'admin_email': __expectString,
        'contact_email': __expectString,
        'contact_phone': __expectString,
        'country_code': __expectString,
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'id': __expectString,
        'name': __expectString,
        'sector': __expectString,
        'status': __expectString,
        'updated_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'updated_by': __expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_UpdateOverrideCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'context_id': __expectString,
        'description': __expectString,
        'override_id': __expectString,
        'weight': __expectString,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_UpdateOverridesExperimentCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'chosen_variant': __expectString,
        'context': _ => de_Condition(_, context),
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'id': __expectString,
        'last_modified': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'name': __expectString,
        'override_keys': _json,
        'status': __expectString,
        'traffic_percentage': __expectInt32,
        'variants': _ => de_ListVariant(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_UpdateTypeTemplatesCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'change_reason': __expectString,
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'type_name': __expectString,
        'type_schema': _ => de_Document(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_UpdateWorkspaceCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'mandatory_dimensions': _json,
        'organisation_id': __expectString,
        'organisation_name': __expectString,
        'workspace_admin_email': __expectString,
        'workspace_name': __expectString,
        'workspace_schema_name': __expectString,
        'workspace_status': __expectString,
        'workspace_strict_mode': __expectBoolean,
    });
    Object.assign(contents, doc);
    return contents;
};
export const de_WeightRecomputeCommand = async (output, context) => {
    if (output.statusCode !== 200 && output.statusCode >= 300) {
        return de_CommandError(output, context);
    }
    const contents = map({
        $metadata: deserializeMetadata(output),
    });
    const data = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
    const doc = take(data, {
        'data': _ => de_WeightRecomputeResponses(_, context),
    });
    Object.assign(contents, doc);
    return contents;
};
const de_CommandError = async (output, context) => {
    const parsedOutput = {
        ...output,
        body: await parseErrorBody(output.body, context)
    };
    const errorCode = loadRestJsonErrorCode(output, parsedOutput.body);
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
const throwDefaultError = withBaseException(__BaseException);
const de_FunctionNotFoundRes = async (parsedOutput, context) => {
    const contents = map({});
    const data = parsedOutput.body;
    const doc = take(data, {});
    Object.assign(contents, doc);
    const exception = new FunctionNotFound({
        $metadata: deserializeMetadata(parsedOutput),
        ...contents
    });
    return __decorateServiceException(exception, parsedOutput.body);
};
const de_InternalServerErrorRes = async (parsedOutput, context) => {
    const contents = map({});
    const data = parsedOutput.body;
    const doc = take(data, {
        'message': __expectString,
    });
    Object.assign(contents, doc);
    const exception = new InternalServerError({
        $metadata: deserializeMetadata(parsedOutput),
        ...contents
    });
    return __decorateServiceException(exception, parsedOutput.body);
};
const de_OrganisationNotFoundRes = async (parsedOutput, context) => {
    const contents = map({});
    const data = parsedOutput.body;
    const doc = take(data, {});
    Object.assign(contents, doc);
    const exception = new OrganisationNotFound({
        $metadata: deserializeMetadata(parsedOutput),
        ...contents
    });
    return __decorateServiceException(exception, parsedOutput.body);
};
const de_ResourceNotFoundRes = async (parsedOutput, context) => {
    const contents = map({});
    const data = parsedOutput.body;
    const doc = take(data, {});
    Object.assign(contents, doc);
    const exception = new ResourceNotFound({
        $metadata: deserializeMetadata(parsedOutput),
        ...contents
    });
    return __decorateServiceException(exception, parsedOutput.body);
};
const de_TypeTemplatesNotFoundRes = async (parsedOutput, context) => {
    const contents = map({});
    const data = parsedOutput.body;
    const doc = take(data, {});
    Object.assign(contents, doc);
    const exception = new TypeTemplatesNotFound({
        $metadata: deserializeMetadata(parsedOutput),
        ...contents
    });
    return __decorateServiceException(exception, parsedOutput.body);
};
const de_WorkspaceNotFoundRes = async (parsedOutput, context) => {
    const contents = map({});
    const data = parsedOutput.body;
    const doc = take(data, {});
    Object.assign(contents, doc);
    const exception = new WorkspaceNotFound({
        $metadata: deserializeMetadata(parsedOutput),
        ...contents
    });
    return __decorateServiceException(exception, parsedOutput.body);
};
const se_AutocompleteFunctionRequest = (input, context) => {
    return take(input, {
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
    return take(input, {
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
    return ContextAction.visit(input, {
        DELETE: value => ({ "DELETE": value }),
        MOVE: value => ({ "MOVE": se_ContextMove(value, context) }),
        PUT: value => ({ "PUT": se_ContextPut(value, context) }),
        REPLACE: value => ({ "REPLACE": se_ContextPut(value, context) }),
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
    return take(input, {
        'change_reason': [],
        'context': _ => se_Condition(_, context),
        'description': [],
        'id': [],
    });
};
const se_ContextPut = (input, context) => {
    return take(input, {
        'change_reason': [],
        'context': _ => se_Condition(_, context),
        'description': [],
        'override': _ => se_Overrides(_, context),
    });
};
const se_FunctionExecutionRequest = (input, context) => {
    return FunctionExecutionRequest.visit(input, {
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
const se_Overrides = (input, context) => {
    return Object.entries(input).reduce((acc, [key, value]) => {
        if (value === null) {
            return acc;
        }
        acc[key] = se_Document(value, context);
        return acc;
    }, {});
};
const se_ValidateFunctionRequest = (input, context) => {
    return take(input, {
        'key': [],
        'value': _ => se_Document(_, context),
    });
};
const se_Variant = (input, context) => {
    return take(input, {
        'context_id': [],
        'id': [],
        'override_id': [],
        'overrides': _ => se_Document(_, context),
        'variant_type': [],
    });
};
const se_VariantUpdateRequest = (input, context) => {
    return take(input, {
        'id': [],
        'overrides': _ => se_Document(_, context),
    });
};
const se_Document = (input, context) => {
    return input;
};
const de_AuditLogFull = (output, context) => {
    return take(output, {
        'action': __expectString,
        'new_data': (_) => de_Document(_, context),
        'original_data': (_) => de_Document(_, context),
        'query': __expectString,
        'table_name': __expectString,
        'timestamp': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'user_name': __expectString,
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
    return take(output, {
        'change_reason': __expectString,
        'created_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'id': __expectString,
        'last_modified_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'override': (_) => de_Overrides(_, context),
        'override_id': __expectString,
        'value': (_) => de_Condition(_, context),
        'weight': __expectString,
    });
};
const de_ContextList = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_ContextPartial(entry, context);
    });
    return retVal;
};
const de_ContextPartial = (output, context) => {
    return take(output, {
        'condition': (_) => de_Condition(_, context),
        'id': __expectString,
        'override_with_keys': _json,
        'priority': __expectInt32,
        'weight': __expectInt32,
    });
};
const de_DefaultConfigFull = (output, context) => {
    return take(output, {
        'change_reason': __expectString,
        'created_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'function_name': __expectString,
        'key': __expectString,
        'last_modified_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'schema': (_) => de_Document(_, context),
        'value': (_) => de_Document(_, context),
    });
};
const de_DimensionExt = (output, context) => {
    return take(output, {
        'change_reason': __expectString,
        'created_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'dependencies': _json,
        'dependency_graph': (_) => de_Object(_, context),
        'dependents': _json,
        'description': __expectString,
        'dimension': __expectString,
        'function_name': __expectString,
        'last_modified_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'mandatory': __expectBoolean,
        'position': __expectInt32,
        'schema': (_) => de_Document(_, context),
    });
};
const de_DimensionExtList = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_DimensionExt(entry, context);
    });
    return retVal;
};
const de_ExperimentList = (output, context) => {
    const retVal = (output || []).filter((e) => e != null).map((entry) => {
        return de_ExperimentResponse(entry, context);
    });
    return retVal;
};
const de_ExperimentResponse = (output, context) => {
    return take(output, {
        'change_reason': __expectString,
        'chosen_variant': __expectString,
        'context': (_) => de_Condition(_, context),
        'created_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'id': __expectString,
        'last_modified': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'name': __expectString,
        'override_keys': _json,
        'status': __expectString,
        'traffic_percentage': __expectInt32,
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
    return take(output, {
        'change_reason': __expectString,
        'description': __expectString,
        'draft_code': __expectString,
        'draft_edited_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'draft_edited_by': __expectString,
        'draft_runtime_version': __expectString,
        'function_name': __expectString,
        'function_type': __expectString,
        'last_modified_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'published_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'published_by': __expectString,
        'published_code': __expectString,
        'published_runtime_version': __expectString,
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
    return take(output, {
        'config': (_) => de_Document(_, context),
        'config_hash': __expectString,
        'created_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'description': __expectString,
        'id': __expectString,
        'tags': _json,
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
    return take(output, {
        'admin_email': __expectString,
        'contact_email': __expectString,
        'contact_phone': __expectString,
        'country_code': __expectString,
        'created_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'id': __expectString,
        'name': __expectString,
        'sector': __expectString,
        'status': __expectString,
        'updated_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'updated_by': __expectString,
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
    return take(output, {
        'change_reason': __expectString,
        'created_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'description': __expectString,
        'last_modified_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'type_name': __expectString,
        'type_schema': (_) => de_Document(_, context),
    });
};
const de_Variant = (output, context) => {
    return take(output, {
        'context_id': __expectString,
        'id': __expectString,
        'override_id': __expectString,
        'overrides': (_) => de_Document(_, context),
        'variant_type': __expectString,
    });
};
const de_WeightRecomputeResponse = (output, context) => {
    return take(output, {
        'condition': (_) => de_Condition(_, context),
        'id': __expectString,
        'new_weight': __expectString,
        'old_weight': __expectString,
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
    return take(output, {
        'created_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'created_by': __expectString,
        'last_modified_at': (_) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
        'last_modified_by': __expectString,
        'mandatory_dimensions': _json,
        'organisation_id': __expectString,
        'organisation_name': __expectString,
        'workspace_admin_email': __expectString,
        'workspace_name': __expectString,
        'workspace_schema_name': __expectString,
        'workspace_status': __expectString,
        'workspace_strict_mode': __expectBoolean,
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
const collectBodyString = (streamBody, context) => collectBody(streamBody, context).then(body => context.utf8Encoder(body));
const _a = "action";
const _ai = "audit_id";
const _al = "all";
const _c = "count";
const _cb = "created_by";
const _co = "context";
const _cq = "context_query";
const _ct = "config_tags";
const _ei = "experiment_ids";
const _en = "experiment_name";
const _fd = "from_date";
const _lm = "last-modified";
const _lm_ = "last_modified";
const _lmb = "last_modified_by";
const _ms = "merge_strategy";
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
