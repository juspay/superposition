// smithy-typescript generated code
import {
  BulkOperationCommandInput,
  BulkOperationCommandOutput,
} from "../commands/BulkOperationCommand";
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
  GetResolvedConfigCommandInput,
  GetResolvedConfigCommandOutput,
} from "../commands/GetResolvedConfigCommand";
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
  ListVersionsCommandInput,
  ListVersionsCommandOutput,
} from "../commands/ListVersionsCommand";
import {
  MoveContextCommandInput,
  MoveContextCommandOutput,
} from "../commands/MoveContextCommand";
import {
  UpdateDefaultConfigCommandInput,
  UpdateDefaultConfigCommandOutput,
} from "../commands/UpdateDefaultConfigCommand";
import {
  UpdateDimensionCommandInput,
  UpdateDimensionCommandOutput,
} from "../commands/UpdateDimensionCommand";
import {
  UpdateOverrideCommandInput,
  UpdateOverrideCommandOutput,
} from "../commands/UpdateOverrideCommand";
import {
  WeightRecomputeCommandInput,
  WeightRecomputeCommandOutput,
} from "../commands/WeightRecomputeCommand";
import { SuperpositionServiceException as __BaseException } from "../models/SuperpositionServiceException";
import {
  AuditLogFull,
  BulkOperationReq,
  ContextAction,
  ContextFull,
  ContextMove,
  ContextPartial,
  ContextPut,
  DefaultConfigFull,
  DimensionExt,
  InternalServerError,
  ResourceNotFound,
} from "../models/models_0";
import {
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
    'change_reason': [],
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
  });
  b.bp("/config/resolved");
  const query: any = map({
    [_p]: [,input[_p]!],
    [_v]: [,input[_v]!],
    [_sr]: [() => input.show_reasoning !== void 0, () => (input[_sr]!.toString())],
    [_xms]: [,input[_ms]!],
  });
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
  .q(query)
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
    [_fd]: [() => input.from_date !== void 0, () => (__serializeDateTime(input[_fd]!).toString())],
    [_td]: [() => input.to_date !== void 0, () => (__serializeDateTime(input[_td]!).toString())],
    [_ta]: [,input[_t]!],
    [_a]: [,input[_a]!],
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
    [_s]: [() => input.size !== void 0, () => (input[_s]!.toString())],
    [_p]: [,input[_p]!],
    [_so]: [,input[_so]!],
    [_sb]: [,input[_sb]!],
    [_cb]: [,input[_cb]!],
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
    'change_reason': [],
    'description': [],
    'function_name': [],
    'schema': _ => se_Document(_, context),
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
  contents.bulk_operation_output = _json(data);
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
    'context_id': __expectString,
    'description': __expectString,
    'override_id': __expectString,
    'weight': __expectInt32,
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
    'change_reason': __expectString,
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
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
    'override_with_keys': _json,
    'value': _ => de_Condition(_, context),
    'weight': __expectInt32,
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
    'override_with_keys': _json,
    'value': _ => de_Condition(_, context),
    'weight': __expectInt32,
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
    'data': _json,
    'total_items': __expectInt32,
    'total_pages': __expectInt32,
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
    'context_id': __expectString,
    'description': __expectString,
    'override_id': __expectString,
    'weight': __expectInt32,
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
    'change_reason': __expectString,
    'created_at': _ => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
    'created_by': __expectString,
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
    'context_id': __expectString,
    'description': __expectString,
    'override_id': __expectString,
    'weight': __expectInt32,
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
    'condition': _ => de_Condition(_, context),
    'id': __expectString,
    'new_weight': __expectInt32,
    'old_weight': __expectInt32,
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
      REPLACE: value => ({ "REPLACE": se_ContextPut(value, context) }),
      _: (name, value) => ({ name: value } as any)
    });
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
      'condition': _ => se_Condition(_, context),
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
      'condition': _ => se_Condition(_, context),
      'description': [],
      'override': _ => se_Overrides(_, context),
    });
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

  // de_BulkOperationOut omitted.

  // de_BulkOperationOutList omitted.

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

  // de_ContextActionOut omitted.

  /**
   * deserializeAws_restJson1ContextFull
   */
  const de_ContextFull = (
    output: any,
    context: __SerdeContext
  ): ContextFull => {
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
      'override_with_keys': _json,
      'value': (_: any) => de_Condition(_, context),
      'weight': __expectInt32,
    }) as any;
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

  // de_ContextMoveOut omitted.

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

  // de_ContextPutOut omitted.

  /**
   * deserializeAws_restJson1DefaultConfigFull
   */
  const de_DefaultConfigFull = (
    output: any,
    context: __SerdeContext
  ): DefaultConfigFull => {
    return take(output, {
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

  /**
   * deserializeAws_restJson1DimensionExt
   */
  const de_DimensionExt = (
    output: any,
    context: __SerdeContext
  ): DimensionExt => {
    return take(output, {
      'change_reason': __expectString,
      'created_at': (_: any) => __expectNonNull(__parseRfc3339DateTimeWithOffset(_)),
      'created_by': __expectString,
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

  /**
   * deserializeAws_restJson1ListContextOut
   */
  const de_ListContextOut = (
    output: any,
    context: __SerdeContext
  ): (ContextFull)[] => {
    const retVal = (output || []).filter((e: any) => e != null).map((entry: any) => {
      return de_ContextFull(entry, context);
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

  // de_ListVersionsOut omitted.

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

  const _a = "action";
  const _ai = "audit_id";
  const _c = "count";
  const _cb = "created_by";
  const _ct = "config_tags";
  const _fd = "from_date";
  const _lm = "last-modified";
  const _lm_ = "last_modified";
  const _ms = "merge_strategy";
  const _oi = "org_id";
  const _p = "prefix";
  const _pa = "page";
  const _s = "size";
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
