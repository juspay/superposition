// smithy-typescript generated code
import {
  AddMembersToGroupCommandInput,
  AddMembersToGroupCommandOutput,
} from "../commands/AddMembersToGroupCommand";
import {
  ApplicableVariantsCommandInput,
  ApplicableVariantsCommandOutput,
} from "../commands/ApplicableVariantsCommand";
import {
  BulkOperationCommandInput,
  BulkOperationCommandOutput,
} from "../commands/BulkOperationCommand";
import {
  ConcludeExperimentCommandInput,
  ConcludeExperimentCommandOutput,
} from "../commands/ConcludeExperimentCommand";
import {
  CreateContextCommandInput,
  CreateContextCommandOutput,
} from "../commands/CreateContextCommand";
import {
  CreateDefaultConfigCommandInput,
  CreateDefaultConfigCommandOutput,
} from "../commands/CreateDefaultConfigCommand";
import {
  CreateDimensionCommandInput,
  CreateDimensionCommandOutput,
} from "../commands/CreateDimensionCommand";
import {
  CreateExperimentCommandInput,
  CreateExperimentCommandOutput,
} from "../commands/CreateExperimentCommand";
import {
  CreateExperimentGroupCommandInput,
  CreateExperimentGroupCommandOutput,
} from "../commands/CreateExperimentGroupCommand";
import {
  CreateFunctionCommandInput,
  CreateFunctionCommandOutput,
} from "../commands/CreateFunctionCommand";
import {
  CreateOrganisationCommandInput,
  CreateOrganisationCommandOutput,
} from "../commands/CreateOrganisationCommand";
import {
  CreateTypeTemplatesCommandInput,
  CreateTypeTemplatesCommandOutput,
} from "../commands/CreateTypeTemplatesCommand";
import {
  CreateWebhookCommandInput,
  CreateWebhookCommandOutput,
} from "../commands/CreateWebhookCommand";
import {
  CreateWorkspaceCommandInput,
  CreateWorkspaceCommandOutput,
} from "../commands/CreateWorkspaceCommand";
import {
  DeleteContextCommandInput,
  DeleteContextCommandOutput,
} from "../commands/DeleteContextCommand";
import {
  DeleteDefaultConfigCommandInput,
  DeleteDefaultConfigCommandOutput,
} from "../commands/DeleteDefaultConfigCommand";
import {
  DeleteDimensionCommandInput,
  DeleteDimensionCommandOutput,
} from "../commands/DeleteDimensionCommand";
import {
  DeleteExperimentGroupCommandInput,
  DeleteExperimentGroupCommandOutput,
} from "../commands/DeleteExperimentGroupCommand";
import {
  DeleteFunctionCommandInput,
  DeleteFunctionCommandOutput,
} from "../commands/DeleteFunctionCommand";
import {
  DeleteTypeTemplatesCommandInput,
  DeleteTypeTemplatesCommandOutput,
} from "../commands/DeleteTypeTemplatesCommand";
import {
  DiscardExperimentCommandInput,
  DiscardExperimentCommandOutput,
} from "../commands/DiscardExperimentCommand";
import {
  GetConfigCommandInput,
  GetConfigCommandOutput,
} from "../commands/GetConfigCommand";
import {
  GetConfigFastCommandInput,
  GetConfigFastCommandOutput,
} from "../commands/GetConfigFastCommand";
import {
  GetContextCommandInput,
  GetContextCommandOutput,
} from "../commands/GetContextCommand";
import {
  GetContextFromConditionCommandInput,
  GetContextFromConditionCommandOutput,
} from "../commands/GetContextFromConditionCommand";
import {
  GetDimensionCommandInput,
  GetDimensionCommandOutput,
} from "../commands/GetDimensionCommand";
import {
  GetExperimentCommandInput,
  GetExperimentCommandOutput,
} from "../commands/GetExperimentCommand";
import {
  GetExperimentGroupCommandInput,
  GetExperimentGroupCommandOutput,
} from "../commands/GetExperimentGroupCommand";
import {
  GetFunctionCommandInput,
  GetFunctionCommandOutput,
} from "../commands/GetFunctionCommand";
import {
  GetOrganisationCommandInput,
  GetOrganisationCommandOutput,
} from "../commands/GetOrganisationCommand";
import {
  GetResolvedConfigCommandInput,
  GetResolvedConfigCommandOutput,
} from "../commands/GetResolvedConfigCommand";
import {
  GetTypeTemplatesListCommandInput,
  GetTypeTemplatesListCommandOutput,
} from "../commands/GetTypeTemplatesListCommand";
import {
  GetWebhookCommandInput,
  GetWebhookCommandOutput,
} from "../commands/GetWebhookCommand";
import {
  ListAuditLogsCommandInput,
  ListAuditLogsCommandOutput,
} from "../commands/ListAuditLogsCommand";
import {
  ListContextsCommandInput,
  ListContextsCommandOutput,
} from "../commands/ListContextsCommand";
import {
  ListDefaultConfigsCommandInput,
  ListDefaultConfigsCommandOutput,
} from "../commands/ListDefaultConfigsCommand";
import {
  ListDimensionsCommandInput,
  ListDimensionsCommandOutput,
} from "../commands/ListDimensionsCommand";
import {
  ListExperimentCommandInput,
  ListExperimentCommandOutput,
} from "../commands/ListExperimentCommand";
import {
  ListExperimentGroupsCommandInput,
  ListExperimentGroupsCommandOutput,
} from "../commands/ListExperimentGroupsCommand";
import {
  ListFunctionCommandInput,
  ListFunctionCommandOutput,
} from "../commands/ListFunctionCommand";
import {
  ListOrganisationCommandInput,
  ListOrganisationCommandOutput,
} from "../commands/ListOrganisationCommand";
import {
  ListVersionsCommandInput,
  ListVersionsCommandOutput,
} from "../commands/ListVersionsCommand";
import {
  ListWebhookCommandInput,
  ListWebhookCommandOutput,
} from "../commands/ListWebhookCommand";
import {
  ListWorkspaceCommandInput,
  ListWorkspaceCommandOutput,
} from "../commands/ListWorkspaceCommand";
import {
  MigrateWorkspaceSchemaCommandInput,
  MigrateWorkspaceSchemaCommandOutput,
} from "../commands/MigrateWorkspaceSchemaCommand";
import {
  MoveContextCommandInput,
  MoveContextCommandOutput,
} from "../commands/MoveContextCommand";
import {
  PauseExperimentCommandInput,
  PauseExperimentCommandOutput,
} from "../commands/PauseExperimentCommand";
import {
  PublishCommandInput,
  PublishCommandOutput,
} from "../commands/PublishCommand";
import {
  RampExperimentCommandInput,
  RampExperimentCommandOutput,
} from "../commands/RampExperimentCommand";
import {
  RemoveMembersFromGroupCommandInput,
  RemoveMembersFromGroupCommandOutput,
} from "../commands/RemoveMembersFromGroupCommand";
import {
  ResumeExperimentCommandInput,
  ResumeExperimentCommandOutput,
} from "../commands/ResumeExperimentCommand";
import {
  TestCommandInput,
  TestCommandOutput,
} from "../commands/TestCommand";
import {
  UpdateDefaultConfigCommandInput,
  UpdateDefaultConfigCommandOutput,
} from "../commands/UpdateDefaultConfigCommand";
import {
  UpdateDimensionCommandInput,
  UpdateDimensionCommandOutput,
} from "../commands/UpdateDimensionCommand";
import {
  UpdateExperimentGroupCommandInput,
  UpdateExperimentGroupCommandOutput,
} from "../commands/UpdateExperimentGroupCommand";
import {
  UpdateFunctionCommandInput,
  UpdateFunctionCommandOutput,
} from "../commands/UpdateFunctionCommand";
import {
  UpdateOrganisationCommandInput,
  UpdateOrganisationCommandOutput,
} from "../commands/UpdateOrganisationCommand";
import {
  UpdateOverrideCommandInput,
  UpdateOverrideCommandOutput,
} from "../commands/UpdateOverrideCommand";
import {
  UpdateOverridesExperimentCommandInput,
  UpdateOverridesExperimentCommandOutput,
} from "../commands/UpdateOverridesExperimentCommand";
import {
  UpdateTypeTemplatesCommandInput,
  UpdateTypeTemplatesCommandOutput,
} from "../commands/UpdateTypeTemplatesCommand";
import {
  UpdateWebhookCommandInput,
  UpdateWebhookCommandOutput,
} from "../commands/UpdateWebhookCommand";
import {
  UpdateWorkspaceCommandInput,
  UpdateWorkspaceCommandOutput,
} from "../commands/UpdateWorkspaceCommand";
import {
  WeightRecomputeCommandInput,
  WeightRecomputeCommandOutput,
} from "../commands/WeightRecomputeCommand";
import { SuperpositionServiceException as __BaseException } from "../models/SuperpositionServiceException";
import {
  AuditLogFull,
  AutocompleteFunctionRequest,
  Bucket,
  BulkOperationOut,
  BulkOperationReq,
  ContextAction,
  ContextActionOut,
  ContextIdentifier,
  ContextMove,
  ContextPartial,
  ContextPut,
  ContextResponse,
  DefaultConfigFull,
  DimensionExt,
  ExperimentGroupResponse,
  ExperimentResponse,
  FunctionExecutionRequest,
  FunctionNotFound,
  FunctionResponse,
  InternalServerError,
  ListVersionsMember,
  OrganisationNotFound,
  OrganisationResponse,
  ResourceNotFound,
  TypeTemplatesNotFound,
  TypeTemplatesResponse,
  UpdateContextOverrideRequest,
  ValidateFunctionRequest,
  Variant,
  VariantUpdateRequest,
  WebhookNotFound,
  WebhookResponse,
  WeightRecomputeResponse,
  WorkspaceNotFound,
  WorkspaceResponse,
} from "../models/models_0";
import {
  awsExpectUnion as __expectUnion,
  loadRestJsonErrorCode,
  parseJsonBody as parseBody,
  parseJsonErrorBody as parseErrorBody,
} from "@aws-sdk/core";
import { requestBuilder as rb } from "@smithy/core";
import {
  HttpRequest as __HttpRequest,
  HttpResponse as __HttpResponse,
} from "@smithy/protocol-http";
import {
  decorateServiceException as __decorateServiceException,
  expectBoolean as __expectBoolean,
  expectInt32 as __expectInt32,
  expectLong as __expectLong,
  expectNonNull as __expectNonNull,
  expectObject as __expectObject,
  expectString as __expectString,
  extendedEncodeURIComponent as __extendedEncodeURIComponent,
  parseRfc3339DateTimeWithOffset as __parseRfc3339DateTimeWithOffset,
  resolvedPath as __resolvedPath,
  serializeDateTime as __serializeDateTime,
  _json,
  collectBody,
  isSerializableHeaderValue,
  map,
  take,
  withBaseException,
} from "@smithy/smithy-client";
import {
  DocumentType as __DocumentType,
  Endpoint as __Endpoint,
  ResponseMetadata as __ResponseMetadata,
  SerdeContext as __SerdeContext,
} from "@smithy/types";

/**
 * serializeAws_restJson1AddMembersToGroupCommand
 */
export const se_AddMembersToGroupCommand = async(
  input: AddMembersToGroupCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiment-groups/{id}/add-members");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
  body = JSON.stringify(take(input, {
    'change_reason': [],
    'member_experiment_ids': _ => _json(_),
  }));
  b.m("PATCH")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1ApplicableVariantsCommand
 */
export const se_ApplicableVariantsCommand = async(
  input: ApplicableVariantsCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiments/applicable-variants");
  let body: any;
  body = JSON.stringify(take(input, {
    'context': _ => se_Condition(_, context),
    'identifier': [],
  }));
  b.m("POST")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1BulkOperationCommand
 */
export const se_BulkOperationCommand = async(
  input: BulkOperationCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
    [_xct]: input[_ct]!,
  });
  b.bp("/context/bulk-operations");
  let body: any;
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
}

/**
 * serializeAws_restJson1ConcludeExperimentCommand
 */
export const se_ConcludeExperimentCommand = async(
  input: ConcludeExperimentCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiments/{id}/conclude");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
  body = JSON.stringify(take(input, {
    'change_reason': [],
    'chosen_variant': [],
    'description': [],
  }));
  b.m("PATCH")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1CreateContextCommand
 */
export const se_CreateContextCommand = async(
  input: CreateContextCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
    [_xct]: input[_ct]!,
  });
  b.bp("/context");
  let body: any;
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
}

/**
 * serializeAws_restJson1CreateDefaultConfigCommand
 */
export const se_CreateDefaultConfigCommand = async(
  input: CreateDefaultConfigCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/default-config");
  let body: any;
  body = JSON.stringify(take(input, {
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
}

/**
 * serializeAws_restJson1CreateDimensionCommand
 */
export const se_CreateDimensionCommand = async(
  input: CreateDimensionCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/dimension");
  let body: any;
  body = JSON.stringify(take(input, {
    'autocomplete_function_name': [],
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
}

/**
 * serializeAws_restJson1CreateExperimentCommand
 */
export const se_CreateExperimentCommand = async(
  input: CreateExperimentCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiments");
  let body: any;
  body = JSON.stringify(take(input, {
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
}

/**
 * serializeAws_restJson1CreateExperimentGroupCommand
 */
export const se_CreateExperimentGroupCommand = async(
  input: CreateExperimentGroupCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiment-groups");
  let body: any;
  body = JSON.stringify(take(input, {
    'change_reason': [],
    'context': _ => se_Condition(_, context),
    'description': [],
    'member_experiment_ids': _ => _json(_),
    'name': [],
    'traffic_percentage': [],
  }));
  b.m("POST")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1CreateFunctionCommand
 */
export const se_CreateFunctionCommand = async(
  input: CreateFunctionCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/function");
  let body: any;
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
}

/**
 * serializeAws_restJson1CreateOrganisationCommand
 */
export const se_CreateOrganisationCommand = async(
  input: CreateOrganisationCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = {
    'content-type': 'application/json',
  };
  b.bp("/superposition/organisations");
  let body: any;
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
}

/**
 * serializeAws_restJson1CreateTypeTemplatesCommand
 */
export const se_CreateTypeTemplatesCommand = async(
  input: CreateTypeTemplatesCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/types");
  let body: any;
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
}

/**
 * serializeAws_restJson1CreateWebhookCommand
 */
export const se_CreateWebhookCommand = async(
  input: CreateWebhookCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/webhook");
  let body: any;
  body = JSON.stringify(take(input, {
    'change_reason': [],
    'custom_headers': _ => se_Object(_, context),
    'description': [],
    'enabled': [],
    'events': _ => _json(_),
    'method': [],
    'name': [],
    'url': [],
    'version': [],
  }));
  b.m("POST")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1CreateWorkspaceCommand
 */
export const se_CreateWorkspaceCommand = async(
  input: CreateWorkspaceCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xoi]: input[_oi]!,
  });
  b.bp("/workspaces");
  let body: any;
  body = JSON.stringify(take(input, {
    'allow_experiment_self_approval': [],
    'auto_populate_control': [],
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
}

/**
 * serializeAws_restJson1DeleteContextCommand
 */
export const se_DeleteContextCommand = async(
  input: DeleteContextCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
    [_xct]: input[_ct]!,
  });
  b.bp("/context/{id}");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
  b.m("DELETE")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1DeleteDefaultConfigCommand
 */
export const se_DeleteDefaultConfigCommand = async(
  input: DeleteDefaultConfigCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/default-config/{key}");
  b.p('key', () => input.key!, '{key}', false)
  let body: any;
  b.m("DELETE")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1DeleteDimensionCommand
 */
export const se_DeleteDimensionCommand = async(
  input: DeleteDimensionCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/dimension/{dimension}");
  b.p('dimension', () => input.dimension!, '{dimension}', false)
  let body: any;
  b.m("DELETE")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1DeleteExperimentGroupCommand
 */
export const se_DeleteExperimentGroupCommand = async(
  input: DeleteExperimentGroupCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiment-groups/{id}");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
  b.m("DELETE")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1DeleteFunctionCommand
 */
export const se_DeleteFunctionCommand = async(
  input: DeleteFunctionCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/function/{function_name}");
  b.p('function_name', () => input.function_name!, '{function_name}', false)
  let body: any;
  b.m("DELETE")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1DeleteTypeTemplatesCommand
 */
export const se_DeleteTypeTemplatesCommand = async(
  input: DeleteTypeTemplatesCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/types/{type_name}");
  b.p('type_name', () => input.type_name!, '{type_name}', false)
  let body: any;
  b.m("DELETE")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1DiscardExperimentCommand
 */
export const se_DiscardExperimentCommand = async(
  input: DiscardExperimentCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiments/{id}/discard");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
  body = JSON.stringify(take(input, {
    'change_reason': [],
  }));
  b.m("PATCH")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1GetConfigCommand
 */
export const se_GetConfigCommand = async(
  input: GetConfigCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/config");
  const query: any = map({
    [_p]: [,input[_p]!],
    [_v]: [,input[_v]!],
  });
  let body: any;
  body = JSON.stringify(take(input, {
    'context': _ => se_ContextMap(_, context),
  }));
  b.m("POST")
  .h(headers)
  .q(query)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1GetConfigFastCommand
 */
export const se_GetConfigFastCommand = async(
  input: GetConfigFastCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/config/fast");
  let body: any;
  b.m("GET")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1GetContextCommand
 */
export const se_GetContextCommand = async(
  input: GetContextCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/context/{id}");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
  b.m("GET")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1GetContextFromConditionCommand
 */
export const se_GetContextFromConditionCommand = async(
  input: GetContextFromConditionCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/context/get");
  let body: any;
  if (input.context !== undefined) {
    if (input.context === null) {
      body = "null";
    } else {
      body = input.context;
    }
  }
  body = JSON.stringify(body);
  b.m("POST")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1GetDimensionCommand
 */
export const se_GetDimensionCommand = async(
  input: GetDimensionCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/dimension/{dimension}");
  b.p('dimension', () => input.dimension!, '{dimension}', false)
  let body: any;
  b.m("GET")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1GetExperimentCommand
 */
export const se_GetExperimentCommand = async(
  input: GetExperimentCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiments/{id}");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
  b.m("GET")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1GetExperimentGroupCommand
 */
export const se_GetExperimentGroupCommand = async(
  input: GetExperimentGroupCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiment-groups/{id}");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
  b.m("GET")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1GetFunctionCommand
 */
export const se_GetFunctionCommand = async(
  input: GetFunctionCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/function/{function_name}");
  b.p('function_name', () => input.function_name!, '{function_name}', false)
  let body: any;
  b.m("GET")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1GetOrganisationCommand
 */
export const se_GetOrganisationCommand = async(
  input: GetOrganisationCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = {
  };
  b.bp("/superposition/organisations/{id}");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
  b.m("GET")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1GetResolvedConfigCommand
 */
export const se_GetResolvedConfigCommand = async(
  input: GetResolvedConfigCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
    [_xms]: input[_ms]!,
  });
  b.bp("/config/resolve");
  const query: any = map({
    [_p]: [,input[_p]!],
    [_v]: [,input[_v]!],
    [_sr]: [() => input.show_reasoning !== void 0, () => (input[_sr]!.toString())],
    [_ci]: [,input[_ci]!],
  });
  let body: any;
  body = JSON.stringify(take(input, {
    'context': _ => se_ContextMap(_, context),
  }));
  b.m("POST")
  .h(headers)
  .q(query)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1GetTypeTemplatesListCommand
 */
export const se_GetTypeTemplatesListCommand = async(
  input: GetTypeTemplatesListCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/types");
  const query: any = map({
    [_c]: [() => input.count !== void 0, () => (input[_c]!.toString())],
    [_pa]: [() => input.page !== void 0, () => (input[_pa]!.toString())],
    [_a]: [() => input.all !== void 0, () => (input[_a]!.toString())],
  });
  let body: any;
  b.m("GET")
  .h(headers)
  .q(query)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1GetWebhookCommand
 */
export const se_GetWebhookCommand = async(
  input: GetWebhookCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/webhook/{name}");
  b.p('name', () => input.name!, '{name}', false)
  let body: any;
  b.m("GET")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1ListAuditLogsCommand
 */
export const se_ListAuditLogsCommand = async(
  input: ListAuditLogsCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/audit");
  const query: any = map({
    [_c]: [() => input.count !== void 0, () => (input[_c]!.toString())],
    [_pa]: [() => input.page !== void 0, () => (input[_pa]!.toString())],
    [_a]: [() => input.all !== void 0, () => (input[_a]!.toString())],
    [_fd]: [() => input.from_date !== void 0, () => (__serializeDateTime(input[_fd]!).toString())],
    [_td]: [() => input.to_date !== void 0, () => (__serializeDateTime(input[_td]!).toString())],
    [_ta]: [,input[_t]!],
    [_ac]: [,input[_ac]!],
    [_u]: [,input[_u]!],
  });
  let body: any;
  b.m("GET")
  .h(headers)
  .q(query)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1ListContextsCommand
 */
export const se_ListContextsCommand = async(
  input: ListContextsCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/context/list");
  const query: any = map({
    [_pa]: [() => input.page !== void 0, () => (input[_pa]!.toString())],
    [_c]: [() => input.count !== void 0, () => (input[_c]!.toString())],
    [_p]: [,input[_p]!],
    [_so]: [,input[_so]!],
    [_sb]: [,input[_sb]!],
    [_cb]: [,input[_cb]!],
    [_lmb]: [,input[_lmb]!],
    [_pl]: [,input[_pl]!],
  });
  let body: any;
  b.m("GET")
  .h(headers)
  .q(query)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1ListDefaultConfigsCommand
 */
export const se_ListDefaultConfigsCommand = async(
  input: ListDefaultConfigsCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/default-config");
  const query: any = map({
    [_c]: [() => input.count !== void 0, () => (input[_c]!.toString())],
    [_pa]: [() => input.page !== void 0, () => (input[_pa]!.toString())],
    [_a]: [() => input.all !== void 0, () => (input[_a]!.toString())],
  });
  let body: any;
  b.m("GET")
  .h(headers)
  .q(query)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1ListDimensionsCommand
 */
export const se_ListDimensionsCommand = async(
  input: ListDimensionsCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/dimension");
  const query: any = map({
    [_c]: [() => input.count !== void 0, () => (input[_c]!.toString())],
    [_pa]: [() => input.page !== void 0, () => (input[_pa]!.toString())],
    [_a]: [() => input.all !== void 0, () => (input[_a]!.toString())],
  });
  let body: any;
  b.m("GET")
  .h(headers)
  .q(query)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1ListExperimentCommand
 */
export const se_ListExperimentCommand = async(
  input: ListExperimentCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiments");
  const query: any = map({
    [_pa]: [() => input.page !== void 0, () => (input[_pa]!.toString())],
    [_c]: [() => input.count !== void 0, () => (input[_c]!.toString())],
    [_a]: [() => input.all !== void 0, () => (input[_a]!.toString())],
    [_s]: [,input[_s]!],
    [_fd]: [() => input.from_date !== void 0, () => (__serializeDateTime(input[_fd]!).toString())],
    [_td]: [() => input.to_date !== void 0, () => (__serializeDateTime(input[_td]!).toString())],
    [_en]: [,input[_en]!],
    [_ei]: [,input[_ei]!],
    [_egi]: [,input[_egi]!],
    [_cb]: [,input[_cb]!],
    [_so]: [,input[_so]!],
    [_sb]: [,input[_sb]!],
    [_geo]: [() => input.global_experiments_only !== void 0, () => (input[_geo]!.toString())],
  });
  let body: any;
  b.m("GET")
  .h(headers)
  .q(query)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1ListExperimentGroupsCommand
 */
export const se_ListExperimentGroupsCommand = async(
  input: ListExperimentGroupsCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiment-groups");
  const query: any = map({
    [_pa]: [() => input.page !== void 0, () => (input[_pa]!.toString())],
    [_c]: [() => input.count !== void 0, () => (input[_c]!.toString())],
    [_n]: [,input[_n]!],
    [_cb]: [,input[_cb]!],
    [_lmb]: [,input[_lmb]!],
    [_so]: [,input[_so]!],
    [_sb]: [,input[_sb]!],
    [_a]: [() => input.all !== void 0, () => (input[_a]!.toString())],
    [_gt]: [,input[_gt]!],
  });
  let body: any;
  b.m("GET")
  .h(headers)
  .q(query)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1ListFunctionCommand
 */
export const se_ListFunctionCommand = async(
  input: ListFunctionCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/function");
  const query: any = map({
    [_c]: [() => input.count !== void 0, () => (input[_c]!.toString())],
    [_pa]: [() => input.page !== void 0, () => (input[_pa]!.toString())],
    [_a]: [() => input.all !== void 0, () => (input[_a]!.toString())],
  });
  let body: any;
  b.m("GET")
  .h(headers)
  .q(query)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1ListOrganisationCommand
 */
export const se_ListOrganisationCommand = async(
  input: ListOrganisationCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = {
  };
  b.bp("/superposition/organisations");
  const query: any = map({
    [_c]: [() => input.count !== void 0, () => (input[_c]!.toString())],
    [_pa]: [() => input.page !== void 0, () => (input[_pa]!.toString())],
    [_a]: [() => input.all !== void 0, () => (input[_a]!.toString())],
  });
  let body: any;
  b.m("GET")
  .h(headers)
  .q(query)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1ListVersionsCommand
 */
export const se_ListVersionsCommand = async(
  input: ListVersionsCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/config/versions");
  const query: any = map({
    [_c]: [() => input.count !== void 0, () => (input[_c]!.toString())],
    [_pa]: [() => input.page !== void 0, () => (input[_pa]!.toString())],
  });
  let body: any;
  b.m("GET")
  .h(headers)
  .q(query)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1ListWebhookCommand
 */
export const se_ListWebhookCommand = async(
  input: ListWebhookCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/webhook");
  const query: any = map({
    [_c]: [() => input.count !== void 0, () => (input[_c]!.toString())],
    [_pa]: [() => input.page !== void 0, () => (input[_pa]!.toString())],
    [_a]: [() => input.all !== void 0, () => (input[_a]!.toString())],
  });
  let body: any;
  b.m("GET")
  .h(headers)
  .q(query)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1ListWorkspaceCommand
 */
export const se_ListWorkspaceCommand = async(
  input: ListWorkspaceCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xoi]: input[_oi]!,
  });
  b.bp("/workspaces");
  const query: any = map({
    [_c]: [() => input.count !== void 0, () => (input[_c]!.toString())],
    [_pa]: [() => input.page !== void 0, () => (input[_pa]!.toString())],
    [_a]: [() => input.all !== void 0, () => (input[_a]!.toString())],
  });
  let body: any;
  b.m("GET")
  .h(headers)
  .q(query)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1MigrateWorkspaceSchemaCommand
 */
export const se_MigrateWorkspaceSchemaCommand = async(
  input: MigrateWorkspaceSchemaCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xoi]: input[_oi]!,
  });
  b.bp("/workspaces/{workspace_name}/db/migrate");
  b.p('workspace_name', () => input.workspace_name!, '{workspace_name}', false)
  let body: any;
  b.m("POST")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1MoveContextCommand
 */
export const se_MoveContextCommand = async(
  input: MoveContextCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/context/move/{id}");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
  body = JSON.stringify(take(input, {
    'change_reason': [],
    'context': _ => se_Condition(_, context),
    'description': [],
  }));
  b.m("PUT")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1PauseExperimentCommand
 */
export const se_PauseExperimentCommand = async(
  input: PauseExperimentCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiments/{id}/pause");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
  body = JSON.stringify(take(input, {
    'change_reason': [],
  }));
  b.m("PATCH")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1PublishCommand
 */
export const se_PublishCommand = async(
  input: PublishCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/function/{function_name}/publish");
  b.p('function_name', () => input.function_name!, '{function_name}', false)
  let body: any;
  body = JSON.stringify(take(input, {
    'change_reason': [],
  }));
  b.m("PATCH")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1RampExperimentCommand
 */
export const se_RampExperimentCommand = async(
  input: RampExperimentCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiments/{id}/ramp");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
  body = JSON.stringify(take(input, {
    'change_reason': [],
    'traffic_percentage': [],
  }));
  b.m("PATCH")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1RemoveMembersFromGroupCommand
 */
export const se_RemoveMembersFromGroupCommand = async(
  input: RemoveMembersFromGroupCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiment-groups/{id}/remove-members");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
  body = JSON.stringify(take(input, {
    'change_reason': [],
    'member_experiment_ids': _ => _json(_),
  }));
  b.m("PATCH")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1ResumeExperimentCommand
 */
export const se_ResumeExperimentCommand = async(
  input: ResumeExperimentCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiments/{id}/resume");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
  body = JSON.stringify(take(input, {
    'change_reason': [],
  }));
  b.m("PATCH")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1TestCommand
 */
export const se_TestCommand = async(
  input: TestCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/function/{function_name}/{stage}/test");
  b.p('function_name', () => input.function_name!, '{function_name}', false)
  b.p('stage', () => input.stage!, '{stage}', false)
  let body: any;
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
}

/**
 * serializeAws_restJson1UpdateDefaultConfigCommand
 */
export const se_UpdateDefaultConfigCommand = async(
  input: UpdateDefaultConfigCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/default-config/{key}");
  b.p('key', () => input.key!, '{key}', false)
  let body: any;
  body = JSON.stringify(take(input, {
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
}

/**
 * serializeAws_restJson1UpdateDimensionCommand
 */
export const se_UpdateDimensionCommand = async(
  input: UpdateDimensionCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/dimension/{dimension}");
  b.p('dimension', () => input.dimension!, '{dimension}', false)
  let body: any;
  body = JSON.stringify(take(input, {
    'autocomplete_function_name': [],
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
}

/**
 * serializeAws_restJson1UpdateExperimentGroupCommand
 */
export const se_UpdateExperimentGroupCommand = async(
  input: UpdateExperimentGroupCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiment-groups/{id}");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
  body = JSON.stringify(take(input, {
    'change_reason': [],
    'description': [],
    'traffic_percentage': [],
  }));
  b.m("PATCH")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1UpdateFunctionCommand
 */
export const se_UpdateFunctionCommand = async(
  input: UpdateFunctionCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/function/{function_name}");
  b.p('function_name', () => input.function_name!, '{function_name}', false)
  let body: any;
  body = JSON.stringify(take(input, {
    'change_reason': [],
    'description': [],
    'function': [],
    'runtime_version': [],
  }));
  b.m("PATCH")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1UpdateOrganisationCommand
 */
export const se_UpdateOrganisationCommand = async(
  input: UpdateOrganisationCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = {
    'content-type': 'application/json',
  };
  b.bp("/superposition/organisations/{id}");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
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
}

/**
 * serializeAws_restJson1UpdateOverrideCommand
 */
export const se_UpdateOverrideCommand = async(
  input: UpdateOverrideCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
    [_xct]: input[_ct]!,
  });
  b.bp("/context/overrides");
  let body: any;
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
}

/**
 * serializeAws_restJson1UpdateOverridesExperimentCommand
 */
export const se_UpdateOverridesExperimentCommand = async(
  input: UpdateOverridesExperimentCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/experiments/{id}/overrides");
  b.p('id', () => input.id!, '{id}', false)
  let body: any;
  body = JSON.stringify(take(input, {
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
}

/**
 * serializeAws_restJson1UpdateTypeTemplatesCommand
 */
export const se_UpdateTypeTemplatesCommand = async(
  input: UpdateTypeTemplatesCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/types/{type_name}");
  b.p('type_name', () => input.type_name!, '{type_name}', false)
  let body: any;
  body = JSON.stringify(take(input, {
    'change_reason': [],
    'description': [],
    'type_schema': _ => se_Document(_, context),
  }));
  b.m("PUT")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1UpdateWebhookCommand
 */
export const se_UpdateWebhookCommand = async(
  input: UpdateWebhookCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
  });
  b.bp("/webhook/{name}");
  b.p('name', () => input.name!, '{name}', false)
  let body: any;
  body = JSON.stringify(take(input, {
    'change_reason': [],
    'custom_headers': _ => se_Object(_, context),
    'description': [],
    'enabled': [],
    'events': _ => _json(_),
    'method': [],
    'url': [],
    'version': [],
  }));
  b.m("PATCH")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1UpdateWorkspaceCommand
 */
export const se_UpdateWorkspaceCommand = async(
  input: UpdateWorkspaceCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    'content-type': 'application/json',
    [_xoi]: input[_oi]!,
  });
  b.bp("/workspaces/{workspace_name}");
  b.p('workspace_name', () => input.workspace_name!, '{workspace_name}', false)
  let body: any;
  body = JSON.stringify(take(input, {
    'allow_experiment_self_approval': [],
    'auto_populate_control': [],
    'config_version': [],
    'mandatory_dimensions': _ => _json(_),
    'metrics': _ => se_Document(_, context),
    'workspace_admin_email': [],
    'workspace_status': [],
  }));
  b.m("PUT")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * serializeAws_restJson1WeightRecomputeCommand
 */
export const se_WeightRecomputeCommand = async(
  input: WeightRecomputeCommandInput,
  context: __SerdeContext
): Promise<__HttpRequest> => {
  const b = rb(input, context);
  const headers: any = map({}, isSerializableHeaderValue, {
    [_xt]: input[_wi]!,
    [_xoi]: input[_oi]!,
    [_xct]: input[_ct]!,
  });
  b.bp("/context/weight/recompute");
  let body: any;
  b.m("PUT")
  .h(headers)
  .b(body);
  return b.build();
}

/**
 * deserializeAws_restJson1AddMembersToGroupCommand
 */
export const de_AddMembersToGroupCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<AddMembersToGroupCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'buckets': _ => de_Buckets(_, context),
    'change_reason': __expectString,
    'context': _ => de_Condition(_, context),
    'context_hash': __expectString,
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'description': __expectString,
    'group_type': __expectString,
    'id': __expectString,
    'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'member_experiment_ids': _json,
    'name': __expectString,
    'traffic_percentage': __expectInt32,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1ApplicableVariantsCommand
 */
export const de_ApplicableVariantsCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<ApplicableVariantsCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'data': _ => de_ListVariant(_, context),
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1BulkOperationCommand
 */
export const de_BulkOperationCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<BulkOperationCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> | undefined = __expectObject(await parseBody(output.body, context));
  contents.bulk_operation_output = de_BulkOperationOut(data, context);
  return contents;
}

/**
 * deserializeAws_restJson1ConcludeExperimentCommand
 */
export const de_ConcludeExperimentCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<ConcludeExperimentCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'change_reason': __expectString,
    'chosen_variant': __expectString,
    'context': _ => de_Condition(_, context),
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'description': __expectString,
    'experiment_group_id': __expectString,
    'experiment_type': __expectString,
    'id': __expectString,
    'last_modified': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'metrics': _ => de_Document(_, context),
    'metrics_url': __expectString,
    'name': __expectString,
    'override_keys': _json,
    'started_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'started_by': __expectString,
    'status': __expectString,
    'traffic_percentage': __expectInt32,
    'variants': _ => de_ListVariant(_, context),
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1CreateContextCommand
 */
export const de_CreateContextCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<CreateContextCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
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
}

/**
 * deserializeAws_restJson1CreateDefaultConfigCommand
 */
export const de_CreateDefaultConfigCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<CreateDefaultConfigCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'autocomplete_function_name': __expectString,
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
}

/**
 * deserializeAws_restJson1CreateDimensionCommand
 */
export const de_CreateDimensionCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<CreateDimensionCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'autocomplete_function_name': __expectString,
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
}

/**
 * deserializeAws_restJson1CreateExperimentCommand
 */
export const de_CreateExperimentCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<CreateExperimentCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'change_reason': __expectString,
    'chosen_variant': __expectString,
    'context': _ => de_Condition(_, context),
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'description': __expectString,
    'experiment_group_id': __expectString,
    'experiment_type': __expectString,
    'id': __expectString,
    'last_modified': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'metrics': _ => de_Document(_, context),
    'metrics_url': __expectString,
    'name': __expectString,
    'override_keys': _json,
    'started_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'started_by': __expectString,
    'status': __expectString,
    'traffic_percentage': __expectInt32,
    'variants': _ => de_ListVariant(_, context),
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1CreateExperimentGroupCommand
 */
export const de_CreateExperimentGroupCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<CreateExperimentGroupCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'buckets': _ => de_Buckets(_, context),
    'change_reason': __expectString,
    'context': _ => de_Condition(_, context),
    'context_hash': __expectString,
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'description': __expectString,
    'group_type': __expectString,
    'id': __expectString,
    'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'member_experiment_ids': _json,
    'name': __expectString,
    'traffic_percentage': __expectInt32,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1CreateFunctionCommand
 */
export const de_CreateFunctionCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<CreateFunctionCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
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
}

/**
 * deserializeAws_restJson1CreateOrganisationCommand
 */
export const de_CreateOrganisationCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<CreateOrganisationCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
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
}

/**
 * deserializeAws_restJson1CreateTypeTemplatesCommand
 */
export const de_CreateTypeTemplatesCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<CreateTypeTemplatesCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
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
}

/**
 * deserializeAws_restJson1CreateWebhookCommand
 */
export const de_CreateWebhookCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<CreateWebhookCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'change_reason': __expectString,
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'custom_headers': _ => de_Object(_, context),
    'description': __expectString,
    'enabled': __expectBoolean,
    'events': _json,
    'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'last_triggered_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'max_retries': __expectInt32,
    'method': __expectString,
    'name': __expectString,
    'url': __expectString,
    'version': __expectString,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1CreateWorkspaceCommand
 */
export const de_CreateWorkspaceCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<CreateWorkspaceCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'allow_experiment_self_approval': __expectBoolean,
    'auto_populate_control': __expectBoolean,
    'config_version': __expectString,
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'mandatory_dimensions': _json,
    'metrics': _ => de_Document(_, context),
    'organisation_id': __expectString,
    'organisation_name': __expectString,
    'strict_mode': __expectBoolean,
    'workspace_admin_email': __expectString,
    'workspace_name': __expectString,
    'workspace_schema_name': __expectString,
    'workspace_status': __expectString,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1DeleteContextCommand
 */
export const de_DeleteContextCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<DeleteContextCommandOutput> => {
  if (output.statusCode !== 201 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  await collectBody(output.body, context);
  return contents;
}

/**
 * deserializeAws_restJson1DeleteDefaultConfigCommand
 */
export const de_DeleteDefaultConfigCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<DeleteDefaultConfigCommandOutput> => {
  if (output.statusCode !== 201 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  await collectBody(output.body, context);
  return contents;
}

/**
 * deserializeAws_restJson1DeleteDimensionCommand
 */
export const de_DeleteDimensionCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<DeleteDimensionCommandOutput> => {
  if (output.statusCode !== 201 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  await collectBody(output.body, context);
  return contents;
}

/**
 * deserializeAws_restJson1DeleteExperimentGroupCommand
 */
export const de_DeleteExperimentGroupCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<DeleteExperimentGroupCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'buckets': _ => de_Buckets(_, context),
    'change_reason': __expectString,
    'context': _ => de_Condition(_, context),
    'context_hash': __expectString,
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'description': __expectString,
    'group_type': __expectString,
    'id': __expectString,
    'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'member_experiment_ids': _json,
    'name': __expectString,
    'traffic_percentage': __expectInt32,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1DeleteFunctionCommand
 */
export const de_DeleteFunctionCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<DeleteFunctionCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  await collectBody(output.body, context);
  return contents;
}

/**
 * deserializeAws_restJson1DeleteTypeTemplatesCommand
 */
export const de_DeleteTypeTemplatesCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<DeleteTypeTemplatesCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
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
}

/**
 * deserializeAws_restJson1DiscardExperimentCommand
 */
export const de_DiscardExperimentCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<DiscardExperimentCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'change_reason': __expectString,
    'chosen_variant': __expectString,
    'context': _ => de_Condition(_, context),
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'description': __expectString,
    'experiment_group_id': __expectString,
    'experiment_type': __expectString,
    'id': __expectString,
    'last_modified': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'metrics': _ => de_Document(_, context),
    'metrics_url': __expectString,
    'name': __expectString,
    'override_keys': _json,
    'started_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'started_by': __expectString,
    'status': __expectString,
    'traffic_percentage': __expectInt32,
    'variants': _ => de_ListVariant(_, context),
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1GetConfigCommand
 */
export const de_GetConfigCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<GetConfigCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
    [_v]: [, output.headers[_xcv]],
    [_lm_]: [() => void 0 !== output.headers[_lm], () => __expectNonNull(__parseRfc3339DateTimeWithOffset(output.headers[_lm]))],
    [_ai]: [, output.headers[_xai]],
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'contexts': _ => de_ContextList(_, context),
    'default_configs': _ => de_Object(_, context),
    'overrides': _ => de_OverridesMap(_, context),
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1GetConfigFastCommand
 */
export const de_GetConfigFastCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<GetConfigFastCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
    [_v]: [, output.headers[_xcv]],
    [_lm_]: [() => void 0 !== output.headers[_lm], () => __expectNonNull(__parseRfc3339DateTimeWithOffset(output.headers[_lm]))],
    [_ai]: [, output.headers[_xai]],
  });
  const data: any = await collectBodyString(output.body, context);
  contents.config = data;
  contents.config = JSON.parse(data);
  return contents;
}

/**
 * deserializeAws_restJson1GetContextCommand
 */
export const de_GetContextCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<GetContextCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
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
}

/**
 * deserializeAws_restJson1GetContextFromConditionCommand
 */
export const de_GetContextFromConditionCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<GetContextFromConditionCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
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
}

/**
 * deserializeAws_restJson1GetDimensionCommand
 */
export const de_GetDimensionCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<GetDimensionCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'autocomplete_function_name': __expectString,
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
}

/**
 * deserializeAws_restJson1GetExperimentCommand
 */
export const de_GetExperimentCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<GetExperimentCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'change_reason': __expectString,
    'chosen_variant': __expectString,
    'context': _ => de_Condition(_, context),
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'description': __expectString,
    'experiment_group_id': __expectString,
    'experiment_type': __expectString,
    'id': __expectString,
    'last_modified': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'metrics': _ => de_Document(_, context),
    'metrics_url': __expectString,
    'name': __expectString,
    'override_keys': _json,
    'started_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'started_by': __expectString,
    'status': __expectString,
    'traffic_percentage': __expectInt32,
    'variants': _ => de_ListVariant(_, context),
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1GetExperimentGroupCommand
 */
export const de_GetExperimentGroupCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<GetExperimentGroupCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'buckets': _ => de_Buckets(_, context),
    'change_reason': __expectString,
    'context': _ => de_Condition(_, context),
    'context_hash': __expectString,
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'description': __expectString,
    'group_type': __expectString,
    'id': __expectString,
    'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'member_experiment_ids': _json,
    'name': __expectString,
    'traffic_percentage': __expectInt32,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1GetFunctionCommand
 */
export const de_GetFunctionCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<GetFunctionCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
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
}

/**
 * deserializeAws_restJson1GetOrganisationCommand
 */
export const de_GetOrganisationCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<GetOrganisationCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
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
}

/**
 * deserializeAws_restJson1GetResolvedConfigCommand
 */
export const de_GetResolvedConfigCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<GetResolvedConfigCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
    [_v]: [, output.headers[_xcv]],
    [_lm_]: [() => void 0 !== output.headers[_lm], () => __expectNonNull(__parseRfc3339DateTimeWithOffset(output.headers[_lm]))],
    [_ai]: [, output.headers[_xai]],
  });
  const data: any = await collectBodyString(output.body, context);
  contents.config = data;
  contents.config = JSON.parse(data);
  return contents;
}

/**
 * deserializeAws_restJson1GetTypeTemplatesListCommand
 */
export const de_GetTypeTemplatesListCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<GetTypeTemplatesListCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'data': _ => de_TypeTemplatesList(_, context),
    'total_items': __expectInt32,
    'total_pages': __expectInt32,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1GetWebhookCommand
 */
export const de_GetWebhookCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<GetWebhookCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'change_reason': __expectString,
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'custom_headers': _ => de_Object(_, context),
    'description': __expectString,
    'enabled': __expectBoolean,
    'events': _json,
    'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'last_triggered_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'max_retries': __expectInt32,
    'method': __expectString,
    'name': __expectString,
    'url': __expectString,
    'version': __expectString,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1ListAuditLogsCommand
 */
export const de_ListAuditLogsCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<ListAuditLogsCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'data': _ => de_AuditLogList(_, context),
    'total_items': __expectInt32,
    'total_pages': __expectInt32,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1ListContextsCommand
 */
export const de_ListContextsCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<ListContextsCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'data': _ => de_ListContextOut(_, context),
    'total_items': __expectInt32,
    'total_pages': __expectInt32,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1ListDefaultConfigsCommand
 */
export const de_ListDefaultConfigsCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<ListDefaultConfigsCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'data': _ => de_ListDefaultConfigOut(_, context),
    'total_items': __expectInt32,
    'total_pages': __expectInt32,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1ListDimensionsCommand
 */
export const de_ListDimensionsCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<ListDimensionsCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'data': _ => de_DimensionExtList(_, context),
    'total_items': __expectInt32,
    'total_pages': __expectInt32,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1ListExperimentCommand
 */
export const de_ListExperimentCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<ListExperimentCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'data': _ => de_ExperimentList(_, context),
    'total_items': __expectLong,
    'total_pages': __expectLong,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1ListExperimentGroupsCommand
 */
export const de_ListExperimentGroupsCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<ListExperimentGroupsCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'data': _ => de_ExperimentGroupList(_, context),
    'total_items': __expectLong,
    'total_pages': __expectLong,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1ListFunctionCommand
 */
export const de_ListFunctionCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<ListFunctionCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'data': _ => de_FunctionListResponse(_, context),
    'total_items': __expectInt32,
    'total_pages': __expectInt32,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1ListOrganisationCommand
 */
export const de_ListOrganisationCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<ListOrganisationCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'data': _ => de_OrganisationList(_, context),
    'total_items': __expectInt32,
    'total_pages': __expectInt32,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1ListVersionsCommand
 */
export const de_ListVersionsCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<ListVersionsCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'data': _ => de_ListVersionsOut(_, context),
    'total_items': __expectInt32,
    'total_pages': __expectInt32,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1ListWebhookCommand
 */
export const de_ListWebhookCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<ListWebhookCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'data': _ => de_WebhookList(_, context),
    'total_items': __expectLong,
    'total_pages': __expectLong,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1ListWorkspaceCommand
 */
export const de_ListWorkspaceCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<ListWorkspaceCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'data': _ => de_WorkspaceList(_, context),
    'total_items': __expectLong,
    'total_pages': __expectLong,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1MigrateWorkspaceSchemaCommand
 */
export const de_MigrateWorkspaceSchemaCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<MigrateWorkspaceSchemaCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'allow_experiment_self_approval': __expectBoolean,
    'auto_populate_control': __expectBoolean,
    'config_version': __expectString,
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'mandatory_dimensions': _json,
    'metrics': _ => de_Document(_, context),
    'organisation_id': __expectString,
    'organisation_name': __expectString,
    'strict_mode': __expectBoolean,
    'workspace_admin_email': __expectString,
    'workspace_name': __expectString,
    'workspace_schema_name': __expectString,
    'workspace_status': __expectString,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1MoveContextCommand
 */
export const de_MoveContextCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<MoveContextCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
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
}

/**
 * deserializeAws_restJson1PauseExperimentCommand
 */
export const de_PauseExperimentCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<PauseExperimentCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'change_reason': __expectString,
    'chosen_variant': __expectString,
    'context': _ => de_Condition(_, context),
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'description': __expectString,
    'experiment_group_id': __expectString,
    'experiment_type': __expectString,
    'id': __expectString,
    'last_modified': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'metrics': _ => de_Document(_, context),
    'metrics_url': __expectString,
    'name': __expectString,
    'override_keys': _json,
    'started_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'started_by': __expectString,
    'status': __expectString,
    'traffic_percentage': __expectInt32,
    'variants': _ => de_ListVariant(_, context),
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1PublishCommand
 */
export const de_PublishCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<PublishCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
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
}

/**
 * deserializeAws_restJson1RampExperimentCommand
 */
export const de_RampExperimentCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<RampExperimentCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'change_reason': __expectString,
    'chosen_variant': __expectString,
    'context': _ => de_Condition(_, context),
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'description': __expectString,
    'experiment_group_id': __expectString,
    'experiment_type': __expectString,
    'id': __expectString,
    'last_modified': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'metrics': _ => de_Document(_, context),
    'metrics_url': __expectString,
    'name': __expectString,
    'override_keys': _json,
    'started_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'started_by': __expectString,
    'status': __expectString,
    'traffic_percentage': __expectInt32,
    'variants': _ => de_ListVariant(_, context),
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1RemoveMembersFromGroupCommand
 */
export const de_RemoveMembersFromGroupCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<RemoveMembersFromGroupCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'buckets': _ => de_Buckets(_, context),
    'change_reason': __expectString,
    'context': _ => de_Condition(_, context),
    'context_hash': __expectString,
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'description': __expectString,
    'group_type': __expectString,
    'id': __expectString,
    'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'member_experiment_ids': _json,
    'name': __expectString,
    'traffic_percentage': __expectInt32,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1ResumeExperimentCommand
 */
export const de_ResumeExperimentCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<ResumeExperimentCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'change_reason': __expectString,
    'chosen_variant': __expectString,
    'context': _ => de_Condition(_, context),
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'description': __expectString,
    'experiment_group_id': __expectString,
    'experiment_type': __expectString,
    'id': __expectString,
    'last_modified': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'metrics': _ => de_Document(_, context),
    'metrics_url': __expectString,
    'name': __expectString,
    'override_keys': _json,
    'started_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'started_by': __expectString,
    'status': __expectString,
    'traffic_percentage': __expectInt32,
    'variants': _ => de_ListVariant(_, context),
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1TestCommand
 */
export const de_TestCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<TestCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'fn_output': _ => de_Document(_, context),
    'function_type': __expectString,
    'stdout': __expectString,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1UpdateDefaultConfigCommand
 */
export const de_UpdateDefaultConfigCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<UpdateDefaultConfigCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'autocomplete_function_name': __expectString,
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
}

/**
 * deserializeAws_restJson1UpdateDimensionCommand
 */
export const de_UpdateDimensionCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<UpdateDimensionCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'autocomplete_function_name': __expectString,
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
}

/**
 * deserializeAws_restJson1UpdateExperimentGroupCommand
 */
export const de_UpdateExperimentGroupCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<UpdateExperimentGroupCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'buckets': _ => de_Buckets(_, context),
    'change_reason': __expectString,
    'context': _ => de_Condition(_, context),
    'context_hash': __expectString,
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'description': __expectString,
    'group_type': __expectString,
    'id': __expectString,
    'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'member_experiment_ids': _json,
    'name': __expectString,
    'traffic_percentage': __expectInt32,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1UpdateFunctionCommand
 */
export const de_UpdateFunctionCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<UpdateFunctionCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
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
}

/**
 * deserializeAws_restJson1UpdateOrganisationCommand
 */
export const de_UpdateOrganisationCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<UpdateOrganisationCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
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
}

/**
 * deserializeAws_restJson1UpdateOverrideCommand
 */
export const de_UpdateOverrideCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<UpdateOverrideCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
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
}

/**
 * deserializeAws_restJson1UpdateOverridesExperimentCommand
 */
export const de_UpdateOverridesExperimentCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<UpdateOverridesExperimentCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'change_reason': __expectString,
    'chosen_variant': __expectString,
    'context': _ => de_Condition(_, context),
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'description': __expectString,
    'experiment_group_id': __expectString,
    'experiment_type': __expectString,
    'id': __expectString,
    'last_modified': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'metrics': _ => de_Document(_, context),
    'metrics_url': __expectString,
    'name': __expectString,
    'override_keys': _json,
    'started_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'started_by': __expectString,
    'status': __expectString,
    'traffic_percentage': __expectInt32,
    'variants': _ => de_ListVariant(_, context),
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1UpdateTypeTemplatesCommand
 */
export const de_UpdateTypeTemplatesCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<UpdateTypeTemplatesCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
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
}

/**
 * deserializeAws_restJson1UpdateWebhookCommand
 */
export const de_UpdateWebhookCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<UpdateWebhookCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'change_reason': __expectString,
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'custom_headers': _ => de_Object(_, context),
    'description': __expectString,
    'enabled': __expectBoolean,
    'events': _json,
    'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'last_triggered_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'max_retries': __expectInt32,
    'method': __expectString,
    'name': __expectString,
    'url': __expectString,
    'version': __expectString,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1UpdateWorkspaceCommand
 */
export const de_UpdateWorkspaceCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<UpdateWorkspaceCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'allow_experiment_self_approval': __expectBoolean,
    'auto_populate_control': __expectBoolean,
    'config_version': __expectString,
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
    'last_modified_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'last_modified_by': __expectString,
    'mandatory_dimensions': _json,
    'metrics': _ => de_Document(_, context),
    'organisation_id': __expectString,
    'organisation_name': __expectString,
    'strict_mode': __expectBoolean,
    'workspace_admin_email': __expectString,
    'workspace_name': __expectString,
    'workspace_schema_name': __expectString,
    'workspace_status': __expectString,
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserializeAws_restJson1WeightRecomputeCommand
 */
export const de_WeightRecomputeCommand = async(
  output: __HttpResponse,
  context: __SerdeContext
): Promise<WeightRecomputeCommandOutput> => {
  if (output.statusCode !== 200 && output.statusCode >= 300) {
    return de_CommandError(output, context);
  }
  const contents: any = map({
    $metadata: deserializeMetadata(output),
  });
  const data: Record<string, any> = __expectNonNull((__expectObject(await parseBody(output.body, context))), "body");
  const doc = take(data, {
    'data': _ => de_WeightRecomputeResponses(_, context),
  });
  Object.assign(contents, doc);
  return contents;
}

/**
 * deserialize_Aws_restJson1CommandError
 */
const de_CommandError = async(
  output: __HttpResponse,
  context: __SerdeContext,
): Promise<never> => {
  const parsedOutput: any = {
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
      }) as never
    }
  }

  const throwDefaultError = withBaseException(__BaseException);
  /**
   * deserializeAws_restJson1FunctionNotFoundRes
   */
  const de_FunctionNotFoundRes = async (
    parsedOutput: any,
    context: __SerdeContext
  ): Promise<FunctionNotFound> => {
    const contents: any = map({
    });
    const data: any = parsedOutput.body;
    const doc = take(data, {
    });
    Object.assign(contents, doc);
    const exception = new FunctionNotFound({
      $metadata: deserializeMetadata(parsedOutput),
      ...contents
    });
    return __decorateServiceException(exception, parsedOutput.body);
  };

  /**
   * deserializeAws_restJson1InternalServerErrorRes
   */
  const de_InternalServerErrorRes = async (
    parsedOutput: any,
    context: __SerdeContext
  ): Promise<InternalServerError> => {
    const contents: any = map({
    });
    const data: any = parsedOutput.body;
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

  /**
   * deserializeAws_restJson1OrganisationNotFoundRes
   */
  const de_OrganisationNotFoundRes = async (
    parsedOutput: any,
    context: __SerdeContext
  ): Promise<OrganisationNotFound> => {
    const contents: any = map({
    });
    const data: any = parsedOutput.body;
    const doc = take(data, {
    });
    Object.assign(contents, doc);
    const exception = new OrganisationNotFound({
      $metadata: deserializeMetadata(parsedOutput),
      ...contents
    });
    return __decorateServiceException(exception, parsedOutput.body);
  };

  /**
   * deserializeAws_restJson1ResourceNotFoundRes
   */
  const de_ResourceNotFoundRes = async (
    parsedOutput: any,
    context: __SerdeContext
  ): Promise<ResourceNotFound> => {
    const contents: any = map({
    });
    const data: any = parsedOutput.body;
    const doc = take(data, {
    });
    Object.assign(contents, doc);
    const exception = new ResourceNotFound({
      $metadata: deserializeMetadata(parsedOutput),
      ...contents
    });
    return __decorateServiceException(exception, parsedOutput.body);
  };

  /**
   * deserializeAws_restJson1TypeTemplatesNotFoundRes
   */
  const de_TypeTemplatesNotFoundRes = async (
    parsedOutput: any,
    context: __SerdeContext
  ): Promise<TypeTemplatesNotFound> => {
    const contents: any = map({
    });
    const data: any = parsedOutput.body;
    const doc = take(data, {
    });
    Object.assign(contents, doc);
    const exception = new TypeTemplatesNotFound({
      $metadata: deserializeMetadata(parsedOutput),
      ...contents
    });
    return __decorateServiceException(exception, parsedOutput.body);
  };

  /**
   * deserializeAws_restJson1WebhookNotFoundRes
   */
  const de_WebhookNotFoundRes = async (
    parsedOutput: any,
    context: __SerdeContext
  ): Promise<WebhookNotFound> => {
    const contents: any = map({
    });
    const data: any = parsedOutput.body;
    const doc = take(data, {
    });
    Object.assign(contents, doc);
    const exception = new WebhookNotFound({
      $metadata: deserializeMetadata(parsedOutput),
      ...contents
    });
    return __decorateServiceException(exception, parsedOutput.body);
  };

  /**
   * deserializeAws_restJson1WorkspaceNotFoundRes
   */
  const de_WorkspaceNotFoundRes = async (
    parsedOutput: any,
    context: __SerdeContext
  ): Promise<WorkspaceNotFound> => {
    const contents: any = map({
    });
    const data: any = parsedOutput.body;
    const doc = take(data, {
    });
    Object.assign(contents, doc);
    const exception = new WorkspaceNotFound({
      $metadata: deserializeMetadata(parsedOutput),
      ...contents
    });
    return __decorateServiceException(exception, parsedOutput.body);
  };

  /**
   * serializeAws_restJson1AutocompleteFunctionRequest
   */
  const se_AutocompleteFunctionRequest = (
    input: AutocompleteFunctionRequest,
    context: __SerdeContext
  ): any => {
    return take(input, {
      'environment': _ => se_Document(_, context),
      'name': [],
      'prefix': [],
    });
  }

  /**
   * serializeAws_restJson1BulkOperationList
   */
  const se_BulkOperationList = (
    input: (ContextAction)[],
    context: __SerdeContext
  ): any => {
    return input.filter((e: any) => e != null).map(entry => {
      return se_ContextAction(entry, context);
    });
  }

  /**
   * serializeAws_restJson1BulkOperationReq
   */
  const se_BulkOperationReq = (
    input: BulkOperationReq,
    context: __SerdeContext
  ): any => {
    return take(input, {
      'operations': _ => se_BulkOperationList(_, context),
    });
  }

  /**
   * serializeAws_restJson1Condition
   */
  const se_Condition = (
    input: Record<string, __DocumentType>,
    context: __SerdeContext
  ): any => {
    return Object.entries(input).reduce((acc: Record<string, any>, [key, value]: [string, any]) => {
      if (value === null) {
        return acc;
      }
      acc[key] = se_Document(value, context);
      return acc;
    }, {});
  }

  /**
   * serializeAws_restJson1ContextAction
   */
  const se_ContextAction = (
    input: ContextAction,
    context: __SerdeContext
  ): any => {
    return ContextAction.visit(input, {
      DELETE: value => ({ "DELETE": value }),
      MOVE: value => ({ "MOVE": se_ContextMove(value, context) }),
      PUT: value => ({ "PUT": se_ContextPut(value, context) }),
      REPLACE: value => ({ "REPLACE": se_UpdateContextOverrideRequest(value, context) }),
      _: (name, value) => ({ name: value } as any)
    });
  }

  /**
   * serializeAws_restJson1ContextIdentifier
   */
  const se_ContextIdentifier = (
    input: ContextIdentifier,
    context: __SerdeContext
  ): any => {
    return ContextIdentifier.visit(input, {
      context: value => ({ "context": se_Condition(value, context) }),
      id: value => ({ "id": value }),
      _: (name, value) => ({ name: value } as any)
    });
  }

  /**
   * serializeAws_restJson1ContextMap
   */
  const se_ContextMap = (
    input: Record<string, __DocumentType>,
    context: __SerdeContext
  ): any => {
    return Object.entries(input).reduce((acc: Record<string, any>, [key, value]: [string, any]) => {
      if (value === null) {
        return acc;
      }
      acc[key] = se_Document(value, context);
      return acc;
    }, {});
  }

  /**
   * serializeAws_restJson1ContextMove
   */
  const se_ContextMove = (
    input: ContextMove,
    context: __SerdeContext
  ): any => {
    return take(input, {
      'change_reason': [],
      'context': _ => se_Condition(_, context),
      'description': [],
      'id': [],
    });
  }

  /**
   * serializeAws_restJson1ContextPut
   */
  const se_ContextPut = (
    input: ContextPut,
    context: __SerdeContext
  ): any => {
    return take(input, {
      'change_reason': [],
      'context': _ => se_Condition(_, context),
      'description': [],
      'override': _ => se_Overrides(_, context),
    });
  }

  // se_Dependencies omitted.

  // se_Events omitted.

  /**
   * serializeAws_restJson1FunctionExecutionRequest
   */
  const se_FunctionExecutionRequest = (
    input: FunctionExecutionRequest,
    context: __SerdeContext
  ): any => {
    return FunctionExecutionRequest.visit(input, {
      AutocompleteFunctionRequest: value => ({ "AutocompleteFunctionRequest": se_AutocompleteFunctionRequest(value, context) }),
      ValidateFunctionRequest: value => ({ "ValidateFunctionRequest": se_ValidateFunctionRequest(value, context) }),
      _: (name, value) => ({ name: value } as any)
    });
  }

  // se_ListMandatoryDimensions omitted.

  /**
   * serializeAws_restJson1ListVariant
   */
  const se_ListVariant = (
    input: (Variant)[],
    context: __SerdeContext
  ): any => {
    return input.filter((e: any) => e != null).map(entry => {
      return se_Variant(entry, context);
    });
  }

  /**
   * serializeAws_restJson1ListVariantUpdateRequest
   */
  const se_ListVariantUpdateRequest = (
    input: (VariantUpdateRequest)[],
    context: __SerdeContext
  ): any => {
    return input.filter((e: any) => e != null).map(entry => {
      return se_VariantUpdateRequest(entry, context);
    });
  }

  /**
   * serializeAws_restJson1Object
   */
  const se_Object = (
    input: Record<string, __DocumentType>,
    context: __SerdeContext
  ): any => {
    return Object.entries(input).reduce((acc: Record<string, any>, [key, value]: [string, any]) => {
      if (value === null) {
        return acc;
      }
      acc[key] = se_Document(value, context);
      return acc;
    }, {});
  }

  /**
   * serializeAws_restJson1Overrides
   */
  const se_Overrides = (
    input: Record<string, __DocumentType>,
    context: __SerdeContext
  ): any => {
    return Object.entries(input).reduce((acc: Record<string, any>, [key, value]: [string, any]) => {
      if (value === null) {
        return acc;
      }
      acc[key] = se_Document(value, context);
      return acc;
    }, {});
  }

  // se_StringList omitted.

  /**
   * serializeAws_restJson1UpdateContextOverrideRequest
   */
  const se_UpdateContextOverrideRequest = (
    input: UpdateContextOverrideRequest,
    context: __SerdeContext
  ): any => {
    return take(input, {
      'change_reason': [],
      'context': _ => se_ContextIdentifier(_, context),
      'description': [],
      'override': _ => se_Overrides(_, context),
    });
  }

  /**
   * serializeAws_restJson1ValidateFunctionRequest
   */
  const se_ValidateFunctionRequest = (
    input: ValidateFunctionRequest,
    context: __SerdeContext
  ): any => {
    return take(input, {
      'key': [],
      'value': _ => se_Document(_, context),
    });
  }

  /**
   * serializeAws_restJson1Variant
   */
  const se_Variant = (
    input: Variant,
    context: __SerdeContext
  ): any => {
    return take(input, {
      'context_id': [],
      'id': [],
      'override_id': [],
      'overrides': _ => se_Document(_, context),
      'variant_type': [],
    });
  }

  /**
   * serializeAws_restJson1VariantUpdateRequest
   */
  const se_VariantUpdateRequest = (
    input: VariantUpdateRequest,
    context: __SerdeContext
  ): any => {
    return take(input, {
      'id': [],
      'overrides': _ => se_Document(_, context),
    });
  }

  /**
   * serializeAws_restJson1Document
   */
  const se_Document = (
    input: __DocumentType,
    context: __SerdeContext
  ): any => {
    return input;
  }

  /**
   * deserializeAws_restJson1AuditLogFull
   */
  const de_AuditLogFull = (
    output: any,
    context: __SerdeContext
  ): AuditLogFull => {
    return take(output, {
      'action': __expectString,
      'new_data': (_: any) => de_Document(_, context),
      'original_data': (_: any) => de_Document(_, context),
      'query': __expectString,
      'table_name': __expectString,
      'timestamp': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'user_name': __expectString,
    }) as any;
  }

  /**
   * deserializeAws_restJson1AuditLogList
   */
  const de_AuditLogList = (
    output: any,
    context: __SerdeContext
  ): (AuditLogFull)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_AuditLogFull(entry, context);
    });
    return retVal;
  }

  // de_Bucket omitted.

  /**
   * deserializeAws_restJson1Buckets
   */
  const de_Buckets = (
    output: any,
    context: __SerdeContext
  ): (Bucket)[] => {
    const retVal = (output || []).map((entry: any) => {
      if (entry === null) {
        return null as any;
      }
      return _json(entry);
    });
    return retVal;
  }

  /**
   * deserializeAws_restJson1BulkOperationOut
   */
  const de_BulkOperationOut = (
    output: any,
    context: __SerdeContext
  ): BulkOperationOut => {
    return take(output, {
      'output': (_: any) => de_BulkOperationOutList(_, context),
    }) as any;
  }

  /**
   * deserializeAws_restJson1BulkOperationOutList
   */
  const de_BulkOperationOutList = (
    output: any,
    context: __SerdeContext
  ): (ContextActionOut)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_ContextActionOut(__expectUnion(entry), context);
    });
    return retVal;
  }

  /**
   * deserializeAws_restJson1Condition
   */
  const de_Condition = (
    output: any,
    context: __SerdeContext
  ): Record<string, __DocumentType> => {
    return Object.entries(output).reduce((acc: Record<string, __DocumentType>, [key, value]: [string, any]) => {
      if (value === null) {
        return acc;
      }
      acc[key as string] = de_Document(value, context);
      return acc;

    }, {} as Record<string, __DocumentType>);}

  /**
   * deserializeAws_restJson1ContextActionOut
   */
  const de_ContextActionOut = (
    output: any,
    context: __SerdeContext
  ): ContextActionOut => {
    if (__expectString(output.DELETE) !== undefined) {
      return { DELETE: __expectString(output.DELETE) as any }
    }
    if (output.MOVE != null) {
      return {
        MOVE: de_ContextResponse(output.MOVE, context)
      };
    }
    if (output.PUT != null) {
      return {
        PUT: de_ContextResponse(output.PUT, context)
      };
    }
    if (output.REPLACE != null) {
      return {
        REPLACE: de_ContextResponse(output.REPLACE, context)
      };
    }
    return { $unknown: Object.entries(output)[0] };
  }

  /**
   * deserializeAws_restJson1ContextList
   */
  const de_ContextList = (
    output: any,
    context: __SerdeContext
  ): (ContextPartial)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_ContextPartial(entry, context);
    });
    return retVal;
  }

  /**
   * deserializeAws_restJson1ContextPartial
   */
  const de_ContextPartial = (
    output: any,
    context: __SerdeContext
  ): ContextPartial => {
    return take(output, {
      'condition': (_: any) => de_Condition(_, context),
      'id': __expectString,
      'override_with_keys': _json,
      'priority': __expectInt32,
      'weight': __expectInt32,
    }) as any;
  }

  /**
   * deserializeAws_restJson1ContextResponse
   */
  const de_ContextResponse = (
    output: any,
    context: __SerdeContext
  ): ContextResponse => {
    return take(output, {
      'change_reason': __expectString,
      'created_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'created_by': __expectString,
      'description': __expectString,
      'id': __expectString,
      'last_modified_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'last_modified_by': __expectString,
      'override': (_: any) => de_Overrides(_, context),
      'override_id': __expectString,
      'value': (_: any) => de_Condition(_, context),
      'weight': __expectString,
    }) as any;
  }

  /**
   * deserializeAws_restJson1DefaultConfigFull
   */
  const de_DefaultConfigFull = (
    output: any,
    context: __SerdeContext
  ): DefaultConfigFull => {
    return take(output, {
      'autocomplete_function_name': __expectString,
      'change_reason': __expectString,
      'created_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'created_by': __expectString,
      'description': __expectString,
      'function_name': __expectString,
      'key': __expectString,
      'last_modified_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'last_modified_by': __expectString,
      'schema': (_: any) => de_Document(_, context),
      'value': (_: any) => de_Document(_, context),
    }) as any;
  }

  // de_Dependencies omitted.

  // de_Dependents omitted.

  /**
   * deserializeAws_restJson1DimensionExt
   */
  const de_DimensionExt = (
    output: any,
    context: __SerdeContext
  ): DimensionExt => {
    return take(output, {
      'autocomplete_function_name': __expectString,
      'change_reason': __expectString,
      'created_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'created_by': __expectString,
      'dependencies': _json,
      'dependency_graph': (_: any) => de_Object(_, context),
      'dependents': _json,
      'description': __expectString,
      'dimension': __expectString,
      'function_name': __expectString,
      'last_modified_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'last_modified_by': __expectString,
      'mandatory': __expectBoolean,
      'position': __expectInt32,
      'schema': (_: any) => de_Document(_, context),
    }) as any;
  }

  /**
   * deserializeAws_restJson1DimensionExtList
   */
  const de_DimensionExtList = (
    output: any,
    context: __SerdeContext
  ): (DimensionExt)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_DimensionExt(entry, context);
    });
    return retVal;
  }

  // de_Events omitted.

  /**
   * deserializeAws_restJson1ExperimentGroupList
   */
  const de_ExperimentGroupList = (
    output: any,
    context: __SerdeContext
  ): (ExperimentGroupResponse)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_ExperimentGroupResponse(entry, context);
    });
    return retVal;
  }

  /**
   * deserializeAws_restJson1ExperimentGroupResponse
   */
  const de_ExperimentGroupResponse = (
    output: any,
    context: __SerdeContext
  ): ExperimentGroupResponse => {
    return take(output, {
      'buckets': (_: any) => de_Buckets(_, context),
      'change_reason': __expectString,
      'context': (_: any) => de_Condition(_, context),
      'context_hash': __expectString,
      'created_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'created_by': __expectString,
      'description': __expectString,
      'group_type': __expectString,
      'id': __expectString,
      'last_modified_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'last_modified_by': __expectString,
      'member_experiment_ids': _json,
      'name': __expectString,
      'traffic_percentage': __expectInt32,
    }) as any;
  }

  /**
   * deserializeAws_restJson1ExperimentList
   */
  const de_ExperimentList = (
    output: any,
    context: __SerdeContext
  ): (ExperimentResponse)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_ExperimentResponse(entry, context);
    });
    return retVal;
  }

  /**
   * deserializeAws_restJson1ExperimentResponse
   */
  const de_ExperimentResponse = (
    output: any,
    context: __SerdeContext
  ): ExperimentResponse => {
    return take(output, {
      'change_reason': __expectString,
      'chosen_variant': __expectString,
      'context': (_: any) => de_Condition(_, context),
      'created_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'created_by': __expectString,
      'description': __expectString,
      'experiment_group_id': __expectString,
      'experiment_type': __expectString,
      'id': __expectString,
      'last_modified': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'last_modified_by': __expectString,
      'metrics': (_: any) => de_Document(_, context),
      'metrics_url': __expectString,
      'name': __expectString,
      'override_keys': _json,
      'started_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'started_by': __expectString,
      'status': __expectString,
      'traffic_percentage': __expectInt32,
      'variants': (_: any) => de_ListVariant(_, context),
    }) as any;
  }

  /**
   * deserializeAws_restJson1FunctionListResponse
   */
  const de_FunctionListResponse = (
    output: any,
    context: __SerdeContext
  ): (FunctionResponse)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_FunctionResponse(entry, context);
    });
    return retVal;
  }

  /**
   * deserializeAws_restJson1FunctionResponse
   */
  const de_FunctionResponse = (
    output: any,
    context: __SerdeContext
  ): FunctionResponse => {
    return take(output, {
      'change_reason': __expectString,
      'description': __expectString,
      'draft_code': __expectString,
      'draft_edited_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'draft_edited_by': __expectString,
      'draft_runtime_version': __expectString,
      'function_name': __expectString,
      'function_type': __expectString,
      'last_modified_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'last_modified_by': __expectString,
      'published_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'published_by': __expectString,
      'published_code': __expectString,
      'published_runtime_version': __expectString,
    }) as any;
  }

  /**
   * deserializeAws_restJson1ListContextOut
   */
  const de_ListContextOut = (
    output: any,
    context: __SerdeContext
  ): (ContextResponse)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_ContextResponse(entry, context);
    });
    return retVal;
  }

  /**
   * deserializeAws_restJson1ListDefaultConfigOut
   */
  const de_ListDefaultConfigOut = (
    output: any,
    context: __SerdeContext
  ): (DefaultConfigFull)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_DefaultConfigFull(entry, context);
    });
    return retVal;
  }

  // de_ListMandatoryDimensions omitted.

  // de_ListOverrideKeys omitted.

  /**
   * deserializeAws_restJson1ListVariant
   */
  const de_ListVariant = (
    output: any,
    context: __SerdeContext
  ): (Variant)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_Variant(entry, context);
    });
    return retVal;
  }

  /**
   * deserializeAws_restJson1ListVersionsMember
   */
  const de_ListVersionsMember = (
    output: any,
    context: __SerdeContext
  ): ListVersionsMember => {
    return take(output, {
      'config': (_: any) => de_Document(_, context),
      'config_hash': __expectString,
      'created_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'description': __expectString,
      'id': __expectString,
      'tags': _json,
    }) as any;
  }

  /**
   * deserializeAws_restJson1ListVersionsOut
   */
  const de_ListVersionsOut = (
    output: any,
    context: __SerdeContext
  ): (ListVersionsMember)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_ListVersionsMember(entry, context);
    });
    return retVal;
  }

  /**
   * deserializeAws_restJson1Object
   */
  const de_Object = (
    output: any,
    context: __SerdeContext
  ): Record<string, __DocumentType> => {
    return Object.entries(output).reduce((acc: Record<string, __DocumentType>, [key, value]: [string, any]) => {
      if (value === null) {
        return acc;
      }
      acc[key as string] = de_Document(value, context);
      return acc;

    }, {} as Record<string, __DocumentType>);}

  /**
   * deserializeAws_restJson1OrganisationList
   */
  const de_OrganisationList = (
    output: any,
    context: __SerdeContext
  ): (OrganisationResponse)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_OrganisationResponse(entry, context);
    });
    return retVal;
  }

  /**
   * deserializeAws_restJson1OrganisationResponse
   */
  const de_OrganisationResponse = (
    output: any,
    context: __SerdeContext
  ): OrganisationResponse => {
    return take(output, {
      'admin_email': __expectString,
      'contact_email': __expectString,
      'contact_phone': __expectString,
      'country_code': __expectString,
      'created_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'created_by': __expectString,
      'id': __expectString,
      'name': __expectString,
      'sector': __expectString,
      'status': __expectString,
      'updated_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'updated_by': __expectString,
    }) as any;
  }

  /**
   * deserializeAws_restJson1Overrides
   */
  const de_Overrides = (
    output: any,
    context: __SerdeContext
  ): Record<string, __DocumentType> => {
    return Object.entries(output).reduce((acc: Record<string, __DocumentType>, [key, value]: [string, any]) => {
      if (value === null) {
        return acc;
      }
      acc[key as string] = de_Document(value, context);
      return acc;

    }, {} as Record<string, __DocumentType>);}

  /**
   * deserializeAws_restJson1OverridesMap
   */
  const de_OverridesMap = (
    output: any,
    context: __SerdeContext
  ): Record<string, Record<string, __DocumentType>> => {
    return Object.entries(output).reduce((acc: Record<string, Record<string, __DocumentType>>, [key, value]: [string, any]) => {
      if (value === null) {
        return acc;
      }
      acc[key as string] = de_Overrides(value, context);
      return acc;

    }, {} as Record<string, Record<string, __DocumentType>>);}

  // de_OverrideWithKeys omitted.

  // de_StringList omitted.

  /**
   * deserializeAws_restJson1TypeTemplatesList
   */
  const de_TypeTemplatesList = (
    output: any,
    context: __SerdeContext
  ): (TypeTemplatesResponse)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_TypeTemplatesResponse(entry, context);
    });
    return retVal;
  }

  /**
   * deserializeAws_restJson1TypeTemplatesResponse
   */
  const de_TypeTemplatesResponse = (
    output: any,
    context: __SerdeContext
  ): TypeTemplatesResponse => {
    return take(output, {
      'change_reason': __expectString,
      'created_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'created_by': __expectString,
      'description': __expectString,
      'last_modified_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'last_modified_by': __expectString,
      'type_name': __expectString,
      'type_schema': (_: any) => de_Document(_, context),
    }) as any;
  }

  /**
   * deserializeAws_restJson1Variant
   */
  const de_Variant = (
    output: any,
    context: __SerdeContext
  ): Variant => {
    return take(output, {
      'context_id': __expectString,
      'id': __expectString,
      'override_id': __expectString,
      'overrides': (_: any) => de_Document(_, context),
      'variant_type': __expectString,
    }) as any;
  }

  /**
   * deserializeAws_restJson1WebhookList
   */
  const de_WebhookList = (
    output: any,
    context: __SerdeContext
  ): (WebhookResponse)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_WebhookResponse(entry, context);
    });
    return retVal;
  }

  /**
   * deserializeAws_restJson1WebhookResponse
   */
  const de_WebhookResponse = (
    output: any,
    context: __SerdeContext
  ): WebhookResponse => {
    return take(output, {
      'change_reason': __expectString,
      'created_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'created_by': __expectString,
      'custom_headers': (_: any) => de_Object(_, context),
      'description': __expectString,
      'enabled': __expectBoolean,
      'events': _json,
      'last_modified_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'last_modified_by': __expectString,
      'last_triggered_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'max_retries': __expectInt32,
      'method': __expectString,
      'name': __expectString,
      'url': __expectString,
      'version': __expectString,
    }) as any;
  }

  /**
   * deserializeAws_restJson1WeightRecomputeResponse
   */
  const de_WeightRecomputeResponse = (
    output: any,
    context: __SerdeContext
  ): WeightRecomputeResponse => {
    return take(output, {
      'condition': (_: any) => de_Condition(_, context),
      'id': __expectString,
      'new_weight': __expectString,
      'old_weight': __expectString,
    }) as any;
  }

  /**
   * deserializeAws_restJson1WeightRecomputeResponses
   */
  const de_WeightRecomputeResponses = (
    output: any,
    context: __SerdeContext
  ): (WeightRecomputeResponse)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_WeightRecomputeResponse(entry, context);
    });
    return retVal;
  }

  /**
   * deserializeAws_restJson1WorkspaceList
   */
  const de_WorkspaceList = (
    output: any,
    context: __SerdeContext
  ): (WorkspaceResponse)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_WorkspaceResponse(entry, context);
    });
    return retVal;
  }

  /**
   * deserializeAws_restJson1WorkspaceResponse
   */
  const de_WorkspaceResponse = (
    output: any,
    context: __SerdeContext
  ): WorkspaceResponse => {
    return take(output, {
      'allow_experiment_self_approval': __expectBoolean,
      'auto_populate_control': __expectBoolean,
      'config_version': __expectString,
      'created_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'created_by': __expectString,
      'last_modified_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'last_modified_by': __expectString,
      'mandatory_dimensions': _json,
      'metrics': (_: any) => de_Document(_, context),
      'organisation_id': __expectString,
      'organisation_name': __expectString,
      'strict_mode': __expectBoolean,
      'workspace_admin_email': __expectString,
      'workspace_name': __expectString,
      'workspace_schema_name': __expectString,
      'workspace_status': __expectString,
    }) as any;
  }

  /**
   * deserializeAws_restJson1Document
   */
  const de_Document = (
    output: any,
    context: __SerdeContext
  ): __DocumentType => {
    return output;
  }

  const deserializeMetadata = (output: __HttpResponse): __ResponseMetadata => ({
    httpStatusCode: output.statusCode,
    requestId: output.headers["x-amzn-requestid"] ?? output.headers["x-amzn-request-id"] ?? output.headers["x-amz-request-id"],
    extendedRequestId: output.headers["x-amz-id-2"],
    cfId: output.headers["x-amz-cf-id"],
  });

  // Encode Uint8Array data into string with utf-8.
  const collectBodyString = (streamBody: any, context: __SerdeContext): Promise<string> => collectBody(streamBody, context).then(body => context.utf8Encoder(body))

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
  const _geo = "global_experiments_only";
  const _gt = "group_type";
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
