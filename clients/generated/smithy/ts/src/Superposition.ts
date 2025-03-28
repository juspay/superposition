// smithy-typescript generated code
import {
  SuperpositionClient,
  SuperpositionClientConfig,
} from "./SuperpositionClient";
import {
  BulkOperationCommand,
  BulkOperationCommandInput,
  BulkOperationCommandOutput,
} from "./commands/BulkOperationCommand";
import {
  CreateContextCommand,
  CreateContextCommandInput,
  CreateContextCommandOutput,
} from "./commands/CreateContextCommand";
import {
  CreateDefaultConfigCommand,
  CreateDefaultConfigCommandInput,
  CreateDefaultConfigCommandOutput,
} from "./commands/CreateDefaultConfigCommand";
import {
  CreateDimensionCommand,
  CreateDimensionCommandInput,
  CreateDimensionCommandOutput,
} from "./commands/CreateDimensionCommand";
import {
  DeleteContextCommand,
  DeleteContextCommandInput,
  DeleteContextCommandOutput,
} from "./commands/DeleteContextCommand";
import {
  DeleteDefaultConfigCommand,
  DeleteDefaultConfigCommandInput,
  DeleteDefaultConfigCommandOutput,
} from "./commands/DeleteDefaultConfigCommand";
import {
  DeleteDimensionCommand,
  DeleteDimensionCommandInput,
  DeleteDimensionCommandOutput,
} from "./commands/DeleteDimensionCommand";
import {
  GetConfigCommand,
  GetConfigCommandInput,
  GetConfigCommandOutput,
} from "./commands/GetConfigCommand";
import {
  GetConfigFastCommand,
  GetConfigFastCommandInput,
  GetConfigFastCommandOutput,
} from "./commands/GetConfigFastCommand";
import {
  GetContextCommand,
  GetContextCommandInput,
  GetContextCommandOutput,
} from "./commands/GetContextCommand";
import {
  GetContextFromConditionCommand,
  GetContextFromConditionCommandInput,
  GetContextFromConditionCommandOutput,
} from "./commands/GetContextFromConditionCommand";
import {
  GetResolvedConfigCommand,
  GetResolvedConfigCommandInput,
  GetResolvedConfigCommandOutput,
} from "./commands/GetResolvedConfigCommand";
import {
  ListAuditLogsCommand,
  ListAuditLogsCommandInput,
  ListAuditLogsCommandOutput,
} from "./commands/ListAuditLogsCommand";
import {
  ListContextsCommand,
  ListContextsCommandInput,
  ListContextsCommandOutput,
} from "./commands/ListContextsCommand";
import {
  ListDefaultConfigsCommand,
  ListDefaultConfigsCommandInput,
  ListDefaultConfigsCommandOutput,
} from "./commands/ListDefaultConfigsCommand";
import {
  ListDimensionsCommand,
  ListDimensionsCommandInput,
  ListDimensionsCommandOutput,
} from "./commands/ListDimensionsCommand";
import {
  ListVersionsCommand,
  ListVersionsCommandInput,
  ListVersionsCommandOutput,
} from "./commands/ListVersionsCommand";
import {
  MoveContextCommand,
  MoveContextCommandInput,
  MoveContextCommandOutput,
} from "./commands/MoveContextCommand";
import {
  UpdateDefaultConfigCommand,
  UpdateDefaultConfigCommandInput,
  UpdateDefaultConfigCommandOutput,
} from "./commands/UpdateDefaultConfigCommand";
import {
  UpdateDimensionCommand,
  UpdateDimensionCommandInput,
  UpdateDimensionCommandOutput,
} from "./commands/UpdateDimensionCommand";
import {
  UpdateOverrideCommand,
  UpdateOverrideCommandInput,
  UpdateOverrideCommandOutput,
} from "./commands/UpdateOverrideCommand";
import {
  WeightRecomputeCommand,
  WeightRecomputeCommandInput,
  WeightRecomputeCommandOutput,
} from "./commands/WeightRecomputeCommand";
import { createAggregatedClient } from "@smithy/smithy-client";
import { HttpHandlerOptions as __HttpHandlerOptions } from "@smithy/types";

const commands = {
  BulkOperationCommand,
  CreateContextCommand,
  CreateDefaultConfigCommand,
  CreateDimensionCommand,
  DeleteContextCommand,
  DeleteDefaultConfigCommand,
  DeleteDimensionCommand,
  GetConfigCommand,
  GetConfigFastCommand,
  GetContextCommand,
  GetContextFromConditionCommand,
  GetResolvedConfigCommand,
  ListAuditLogsCommand,
  ListContextsCommand,
  ListDefaultConfigsCommand,
  ListDimensionsCommand,
  ListVersionsCommand,
  MoveContextCommand,
  UpdateDefaultConfigCommand,
  UpdateDimensionCommand,
  UpdateOverrideCommand,
  WeightRecomputeCommand,
}

export interface Superposition {
  /**
   * @see {@link BulkOperationCommand}
   */
  bulkOperation(
    args: BulkOperationCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<BulkOperationCommandOutput>;
  bulkOperation(
    args: BulkOperationCommandInput,
    cb: (err: any, data?: BulkOperationCommandOutput) => void
  ): void;
  bulkOperation(
    args: BulkOperationCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: BulkOperationCommandOutput) => void
  ): void;

  /**
   * @see {@link CreateContextCommand}
   */
  createContext(
    args: CreateContextCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<CreateContextCommandOutput>;
  createContext(
    args: CreateContextCommandInput,
    cb: (err: any, data?: CreateContextCommandOutput) => void
  ): void;
  createContext(
    args: CreateContextCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: CreateContextCommandOutput) => void
  ): void;

  /**
   * @see {@link CreateDefaultConfigCommand}
   */
  createDefaultConfig(
    args: CreateDefaultConfigCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<CreateDefaultConfigCommandOutput>;
  createDefaultConfig(
    args: CreateDefaultConfigCommandInput,
    cb: (err: any, data?: CreateDefaultConfigCommandOutput) => void
  ): void;
  createDefaultConfig(
    args: CreateDefaultConfigCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: CreateDefaultConfigCommandOutput) => void
  ): void;

  /**
   * @see {@link CreateDimensionCommand}
   */
  createDimension(
    args: CreateDimensionCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<CreateDimensionCommandOutput>;
  createDimension(
    args: CreateDimensionCommandInput,
    cb: (err: any, data?: CreateDimensionCommandOutput) => void
  ): void;
  createDimension(
    args: CreateDimensionCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: CreateDimensionCommandOutput) => void
  ): void;

  /**
   * @see {@link DeleteContextCommand}
   */
  deleteContext(
    args: DeleteContextCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<DeleteContextCommandOutput>;
  deleteContext(
    args: DeleteContextCommandInput,
    cb: (err: any, data?: DeleteContextCommandOutput) => void
  ): void;
  deleteContext(
    args: DeleteContextCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: DeleteContextCommandOutput) => void
  ): void;

  /**
   * @see {@link DeleteDefaultConfigCommand}
   */
  deleteDefaultConfig(
    args: DeleteDefaultConfigCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<DeleteDefaultConfigCommandOutput>;
  deleteDefaultConfig(
    args: DeleteDefaultConfigCommandInput,
    cb: (err: any, data?: DeleteDefaultConfigCommandOutput) => void
  ): void;
  deleteDefaultConfig(
    args: DeleteDefaultConfigCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: DeleteDefaultConfigCommandOutput) => void
  ): void;

  /**
   * @see {@link DeleteDimensionCommand}
   */
  deleteDimension(
    args: DeleteDimensionCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<DeleteDimensionCommandOutput>;
  deleteDimension(
    args: DeleteDimensionCommandInput,
    cb: (err: any, data?: DeleteDimensionCommandOutput) => void
  ): void;
  deleteDimension(
    args: DeleteDimensionCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: DeleteDimensionCommandOutput) => void
  ): void;

  /**
   * @see {@link GetConfigCommand}
   */
  getConfig(
    args: GetConfigCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<GetConfigCommandOutput>;
  getConfig(
    args: GetConfigCommandInput,
    cb: (err: any, data?: GetConfigCommandOutput) => void
  ): void;
  getConfig(
    args: GetConfigCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: GetConfigCommandOutput) => void
  ): void;

  /**
   * @see {@link GetConfigFastCommand}
   */
  getConfigFast(
    args: GetConfigFastCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<GetConfigFastCommandOutput>;
  getConfigFast(
    args: GetConfigFastCommandInput,
    cb: (err: any, data?: GetConfigFastCommandOutput) => void
  ): void;
  getConfigFast(
    args: GetConfigFastCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: GetConfigFastCommandOutput) => void
  ): void;

  /**
   * @see {@link GetContextCommand}
   */
  getContext(
    args: GetContextCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<GetContextCommandOutput>;
  getContext(
    args: GetContextCommandInput,
    cb: (err: any, data?: GetContextCommandOutput) => void
  ): void;
  getContext(
    args: GetContextCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: GetContextCommandOutput) => void
  ): void;

  /**
   * @see {@link GetContextFromConditionCommand}
   */
  getContextFromCondition(
    args: GetContextFromConditionCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<GetContextFromConditionCommandOutput>;
  getContextFromCondition(
    args: GetContextFromConditionCommandInput,
    cb: (err: any, data?: GetContextFromConditionCommandOutput) => void
  ): void;
  getContextFromCondition(
    args: GetContextFromConditionCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: GetContextFromConditionCommandOutput) => void
  ): void;

  /**
   * @see {@link GetResolvedConfigCommand}
   */
  getResolvedConfig(
    args: GetResolvedConfigCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<GetResolvedConfigCommandOutput>;
  getResolvedConfig(
    args: GetResolvedConfigCommandInput,
    cb: (err: any, data?: GetResolvedConfigCommandOutput) => void
  ): void;
  getResolvedConfig(
    args: GetResolvedConfigCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: GetResolvedConfigCommandOutput) => void
  ): void;

  /**
   * @see {@link ListAuditLogsCommand}
   */
  listAuditLogs(
    args: ListAuditLogsCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<ListAuditLogsCommandOutput>;
  listAuditLogs(
    args: ListAuditLogsCommandInput,
    cb: (err: any, data?: ListAuditLogsCommandOutput) => void
  ): void;
  listAuditLogs(
    args: ListAuditLogsCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: ListAuditLogsCommandOutput) => void
  ): void;

  /**
   * @see {@link ListContextsCommand}
   */
  listContexts(
    args: ListContextsCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<ListContextsCommandOutput>;
  listContexts(
    args: ListContextsCommandInput,
    cb: (err: any, data?: ListContextsCommandOutput) => void
  ): void;
  listContexts(
    args: ListContextsCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: ListContextsCommandOutput) => void
  ): void;

  /**
   * @see {@link ListDefaultConfigsCommand}
   */
  listDefaultConfigs(
    args: ListDefaultConfigsCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<ListDefaultConfigsCommandOutput>;
  listDefaultConfigs(
    args: ListDefaultConfigsCommandInput,
    cb: (err: any, data?: ListDefaultConfigsCommandOutput) => void
  ): void;
  listDefaultConfigs(
    args: ListDefaultConfigsCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: ListDefaultConfigsCommandOutput) => void
  ): void;

  /**
   * @see {@link ListDimensionsCommand}
   */
  listDimensions(
    args: ListDimensionsCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<ListDimensionsCommandOutput>;
  listDimensions(
    args: ListDimensionsCommandInput,
    cb: (err: any, data?: ListDimensionsCommandOutput) => void
  ): void;
  listDimensions(
    args: ListDimensionsCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: ListDimensionsCommandOutput) => void
  ): void;

  /**
   * @see {@link ListVersionsCommand}
   */
  listVersions(
    args: ListVersionsCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<ListVersionsCommandOutput>;
  listVersions(
    args: ListVersionsCommandInput,
    cb: (err: any, data?: ListVersionsCommandOutput) => void
  ): void;
  listVersions(
    args: ListVersionsCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: ListVersionsCommandOutput) => void
  ): void;

  /**
   * @see {@link MoveContextCommand}
   */
  moveContext(
    args: MoveContextCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<MoveContextCommandOutput>;
  moveContext(
    args: MoveContextCommandInput,
    cb: (err: any, data?: MoveContextCommandOutput) => void
  ): void;
  moveContext(
    args: MoveContextCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: MoveContextCommandOutput) => void
  ): void;

  /**
   * @see {@link UpdateDefaultConfigCommand}
   */
  updateDefaultConfig(
    args: UpdateDefaultConfigCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<UpdateDefaultConfigCommandOutput>;
  updateDefaultConfig(
    args: UpdateDefaultConfigCommandInput,
    cb: (err: any, data?: UpdateDefaultConfigCommandOutput) => void
  ): void;
  updateDefaultConfig(
    args: UpdateDefaultConfigCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: UpdateDefaultConfigCommandOutput) => void
  ): void;

  /**
   * @see {@link UpdateDimensionCommand}
   */
  updateDimension(
    args: UpdateDimensionCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<UpdateDimensionCommandOutput>;
  updateDimension(
    args: UpdateDimensionCommandInput,
    cb: (err: any, data?: UpdateDimensionCommandOutput) => void
  ): void;
  updateDimension(
    args: UpdateDimensionCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: UpdateDimensionCommandOutput) => void
  ): void;

  /**
   * @see {@link UpdateOverrideCommand}
   */
  updateOverride(
    args: UpdateOverrideCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<UpdateOverrideCommandOutput>;
  updateOverride(
    args: UpdateOverrideCommandInput,
    cb: (err: any, data?: UpdateOverrideCommandOutput) => void
  ): void;
  updateOverride(
    args: UpdateOverrideCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: UpdateOverrideCommandOutput) => void
  ): void;

  /**
   * @see {@link WeightRecomputeCommand}
   */
  weightRecompute(
    args: WeightRecomputeCommandInput,
    options?: __HttpHandlerOptions,
  ): Promise<WeightRecomputeCommandOutput>;
  weightRecompute(
    args: WeightRecomputeCommandInput,
    cb: (err: any, data?: WeightRecomputeCommandOutput) => void
  ): void;
  weightRecompute(
    args: WeightRecomputeCommandInput,
    options: __HttpHandlerOptions,
    cb: (err: any, data?: WeightRecomputeCommandOutput) => void
  ): void;

}

/**
 * @public
 */
export class Superposition extends SuperpositionClient implements Superposition {}
createAggregatedClient(commands, Superposition);
