// smithy-typescript generated code
import {
  HttpAuthSchemeInputConfig,
  HttpAuthSchemeResolvedConfig,
  defaultSuperpositionHttpAuthSchemeParametersProvider,
  resolveHttpAuthSchemeConfig,
} from "./auth/httpAuthSchemeProvider";
import {
  AddMembersToGroupCommandInput,
  AddMembersToGroupCommandOutput,
} from "./commands/AddMembersToGroupCommand";
import {
  ApplicableVariantsCommandInput,
  ApplicableVariantsCommandOutput,
} from "./commands/ApplicableVariantsCommand";
import {
  BulkOperationCommandInput,
  BulkOperationCommandOutput,
} from "./commands/BulkOperationCommand";
import {
  ConcludeExperimentCommandInput,
  ConcludeExperimentCommandOutput,
} from "./commands/ConcludeExperimentCommand";
import {
  CreateContextCommandInput,
  CreateContextCommandOutput,
} from "./commands/CreateContextCommand";
import {
  CreateDefaultConfigCommandInput,
  CreateDefaultConfigCommandOutput,
} from "./commands/CreateDefaultConfigCommand";
import {
  CreateDimensionCommandInput,
  CreateDimensionCommandOutput,
} from "./commands/CreateDimensionCommand";
import {
  CreateExperimentCommandInput,
  CreateExperimentCommandOutput,
} from "./commands/CreateExperimentCommand";
import {
  CreateExperimentGroupCommandInput,
  CreateExperimentGroupCommandOutput,
} from "./commands/CreateExperimentGroupCommand";
import {
  CreateFunctionCommandInput,
  CreateFunctionCommandOutput,
} from "./commands/CreateFunctionCommand";
import {
  CreateOrganisationCommandInput,
  CreateOrganisationCommandOutput,
} from "./commands/CreateOrganisationCommand";
import {
  CreateTypeTemplatesCommandInput,
  CreateTypeTemplatesCommandOutput,
} from "./commands/CreateTypeTemplatesCommand";
import {
  CreateWebhookCommandInput,
  CreateWebhookCommandOutput,
} from "./commands/CreateWebhookCommand";
import {
  CreateWorkspaceCommandInput,
  CreateWorkspaceCommandOutput,
} from "./commands/CreateWorkspaceCommand";
import {
  DeleteContextCommandInput,
  DeleteContextCommandOutput,
} from "./commands/DeleteContextCommand";
import {
  DeleteDefaultConfigCommandInput,
  DeleteDefaultConfigCommandOutput,
} from "./commands/DeleteDefaultConfigCommand";
import {
  DeleteDimensionCommandInput,
  DeleteDimensionCommandOutput,
} from "./commands/DeleteDimensionCommand";
import {
  DeleteExperimentGroupCommandInput,
  DeleteExperimentGroupCommandOutput,
} from "./commands/DeleteExperimentGroupCommand";
import {
  DeleteFunctionCommandInput,
  DeleteFunctionCommandOutput,
} from "./commands/DeleteFunctionCommand";
import {
  DeleteTypeTemplatesCommandInput,
  DeleteTypeTemplatesCommandOutput,
} from "./commands/DeleteTypeTemplatesCommand";
import {
  DiscardExperimentCommandInput,
  DiscardExperimentCommandOutput,
} from "./commands/DiscardExperimentCommand";
import {
  GetConfigCommandInput,
  GetConfigCommandOutput,
} from "./commands/GetConfigCommand";
import {
  GetConfigFastCommandInput,
  GetConfigFastCommandOutput,
} from "./commands/GetConfigFastCommand";
import {
  GetContextCommandInput,
  GetContextCommandOutput,
} from "./commands/GetContextCommand";
import {
  GetContextFromConditionCommandInput,
  GetContextFromConditionCommandOutput,
} from "./commands/GetContextFromConditionCommand";
import {
  GetDimensionCommandInput,
  GetDimensionCommandOutput,
} from "./commands/GetDimensionCommand";
import {
  GetExperimentCommandInput,
  GetExperimentCommandOutput,
} from "./commands/GetExperimentCommand";
import {
  GetExperimentGroupCommandInput,
  GetExperimentGroupCommandOutput,
} from "./commands/GetExperimentGroupCommand";
import {
  GetFunctionCommandInput,
  GetFunctionCommandOutput,
} from "./commands/GetFunctionCommand";
import {
  GetOrganisationCommandInput,
  GetOrganisationCommandOutput,
} from "./commands/GetOrganisationCommand";
import {
  GetResolvedConfigCommandInput,
  GetResolvedConfigCommandOutput,
} from "./commands/GetResolvedConfigCommand";
import {
  GetTypeTemplatesListCommandInput,
  GetTypeTemplatesListCommandOutput,
} from "./commands/GetTypeTemplatesListCommand";
import {
  GetWebhookCommandInput,
  GetWebhookCommandOutput,
} from "./commands/GetWebhookCommand";
import {
  ListAuditLogsCommandInput,
  ListAuditLogsCommandOutput,
} from "./commands/ListAuditLogsCommand";
import {
  ListContextsCommandInput,
  ListContextsCommandOutput,
} from "./commands/ListContextsCommand";
import {
  ListDefaultConfigsCommandInput,
  ListDefaultConfigsCommandOutput,
} from "./commands/ListDefaultConfigsCommand";
import {
  ListDimensionsCommandInput,
  ListDimensionsCommandOutput,
} from "./commands/ListDimensionsCommand";
import {
  ListExperimentCommandInput,
  ListExperimentCommandOutput,
} from "./commands/ListExperimentCommand";
import {
  ListExperimentGroupsCommandInput,
  ListExperimentGroupsCommandOutput,
} from "./commands/ListExperimentGroupsCommand";
import {
  ListFunctionCommandInput,
  ListFunctionCommandOutput,
} from "./commands/ListFunctionCommand";
import {
  ListOrganisationCommandInput,
  ListOrganisationCommandOutput,
} from "./commands/ListOrganisationCommand";
import {
  ListVersionsCommandInput,
  ListVersionsCommandOutput,
} from "./commands/ListVersionsCommand";
import {
  ListWebhookCommandInput,
  ListWebhookCommandOutput,
} from "./commands/ListWebhookCommand";
import {
  ListWorkspaceCommandInput,
  ListWorkspaceCommandOutput,
} from "./commands/ListWorkspaceCommand";
import {
  MigrateWorkspaceSchemaCommandInput,
  MigrateWorkspaceSchemaCommandOutput,
} from "./commands/MigrateWorkspaceSchemaCommand";
import {
  MoveContextCommandInput,
  MoveContextCommandOutput,
} from "./commands/MoveContextCommand";
import {
  PauseExperimentCommandInput,
  PauseExperimentCommandOutput,
} from "./commands/PauseExperimentCommand";
import {
  PublishCommandInput,
  PublishCommandOutput,
} from "./commands/PublishCommand";
import {
  RampExperimentCommandInput,
  RampExperimentCommandOutput,
} from "./commands/RampExperimentCommand";
import {
  RemoveMembersFromGroupCommandInput,
  RemoveMembersFromGroupCommandOutput,
} from "./commands/RemoveMembersFromGroupCommand";
import {
  ResumeExperimentCommandInput,
  ResumeExperimentCommandOutput,
} from "./commands/ResumeExperimentCommand";
import {
  TestCommandInput,
  TestCommandOutput,
} from "./commands/TestCommand";
import {
  UpdateDefaultConfigCommandInput,
  UpdateDefaultConfigCommandOutput,
} from "./commands/UpdateDefaultConfigCommand";
import {
  UpdateDimensionCommandInput,
  UpdateDimensionCommandOutput,
} from "./commands/UpdateDimensionCommand";
import {
  UpdateExperimentGroupCommandInput,
  UpdateExperimentGroupCommandOutput,
} from "./commands/UpdateExperimentGroupCommand";
import {
  UpdateFunctionCommandInput,
  UpdateFunctionCommandOutput,
} from "./commands/UpdateFunctionCommand";
import {
  UpdateOrganisationCommandInput,
  UpdateOrganisationCommandOutput,
} from "./commands/UpdateOrganisationCommand";
import {
  UpdateOverrideCommandInput,
  UpdateOverrideCommandOutput,
} from "./commands/UpdateOverrideCommand";
import {
  UpdateOverridesExperimentCommandInput,
  UpdateOverridesExperimentCommandOutput,
} from "./commands/UpdateOverridesExperimentCommand";
import {
  UpdateTypeTemplatesCommandInput,
  UpdateTypeTemplatesCommandOutput,
} from "./commands/UpdateTypeTemplatesCommand";
import {
  UpdateWebhookCommandInput,
  UpdateWebhookCommandOutput,
} from "./commands/UpdateWebhookCommand";
import {
  UpdateWorkspaceCommandInput,
  UpdateWorkspaceCommandOutput,
} from "./commands/UpdateWorkspaceCommand";
import {
  WeightRecomputeCommandInput,
  WeightRecomputeCommandOutput,
} from "./commands/WeightRecomputeCommand";
import { getRuntimeConfig as __getRuntimeConfig } from "./runtimeConfig";
import {
  RuntimeExtension,
  RuntimeExtensionsConfig,
  resolveRuntimeExtensions,
} from "./runtimeExtensions";
import {
  HostHeaderInputConfig,
  HostHeaderResolvedConfig,
  getHostHeaderPlugin,
  resolveHostHeaderConfig,
} from "@aws-sdk/middleware-host-header";
import { getLoggerPlugin } from "@aws-sdk/middleware-logger";
import { getRecursionDetectionPlugin } from "@aws-sdk/middleware-recursion-detection";
import {
  UserAgentInputConfig,
  UserAgentResolvedConfig,
  getUserAgentPlugin,
  resolveUserAgentConfig,
} from "@aws-sdk/middleware-user-agent";
import {
  CustomEndpointsInputConfig,
  CustomEndpointsResolvedConfig,
  resolveCustomEndpointsConfig,
} from "@smithy/config-resolver";
import {
  DefaultIdentityProviderConfig,
  getHttpAuthSchemePlugin,
  getHttpSigningPlugin,
} from "@smithy/core";
import { getContentLengthPlugin } from "@smithy/middleware-content-length";
import {
  RetryInputConfig,
  RetryResolvedConfig,
  getRetryPlugin,
  resolveRetryConfig,
} from "@smithy/middleware-retry";
import { HttpHandlerUserInput as __HttpHandlerUserInput } from "@smithy/protocol-http";
import {
  Client as __Client,
  DefaultsMode as __DefaultsMode,
  SmithyConfiguration as __SmithyConfiguration,
  SmithyResolvedConfiguration as __SmithyResolvedConfiguration,
} from "@smithy/smithy-client";
import {
  Provider,
  BodyLengthCalculator as __BodyLengthCalculator,
  CheckOptionalClientConfig as __CheckOptionalClientConfig,
  ChecksumConstructor as __ChecksumConstructor,
  Decoder as __Decoder,
  Encoder as __Encoder,
  HashConstructor as __HashConstructor,
  HttpHandlerOptions as __HttpHandlerOptions,
  Logger as __Logger,
  Provider as __Provider,
  StreamCollector as __StreamCollector,
  UrlParser as __UrlParser,
  UserAgent as __UserAgent,
} from "@smithy/types";

export { __Client }

/**
 * @public
 */
export type ServiceInputTypes =
  | AddMembersToGroupCommandInput
  | ApplicableVariantsCommandInput
  | BulkOperationCommandInput
  | ConcludeExperimentCommandInput
  | CreateContextCommandInput
  | CreateDefaultConfigCommandInput
  | CreateDimensionCommandInput
  | CreateExperimentCommandInput
  | CreateExperimentGroupCommandInput
  | CreateFunctionCommandInput
  | CreateOrganisationCommandInput
  | CreateTypeTemplatesCommandInput
  | CreateWebhookCommandInput
  | CreateWorkspaceCommandInput
  | DeleteContextCommandInput
  | DeleteDefaultConfigCommandInput
  | DeleteDimensionCommandInput
  | DeleteExperimentGroupCommandInput
  | DeleteFunctionCommandInput
  | DeleteTypeTemplatesCommandInput
  | DiscardExperimentCommandInput
  | GetConfigCommandInput
  | GetConfigFastCommandInput
  | GetContextCommandInput
  | GetContextFromConditionCommandInput
  | GetDimensionCommandInput
  | GetExperimentCommandInput
  | GetExperimentGroupCommandInput
  | GetFunctionCommandInput
  | GetOrganisationCommandInput
  | GetResolvedConfigCommandInput
  | GetTypeTemplatesListCommandInput
  | GetWebhookCommandInput
  | ListAuditLogsCommandInput
  | ListContextsCommandInput
  | ListDefaultConfigsCommandInput
  | ListDimensionsCommandInput
  | ListExperimentCommandInput
  | ListExperimentGroupsCommandInput
  | ListFunctionCommandInput
  | ListOrganisationCommandInput
  | ListVersionsCommandInput
  | ListWebhookCommandInput
  | ListWorkspaceCommandInput
  | MigrateWorkspaceSchemaCommandInput
  | MoveContextCommandInput
  | PauseExperimentCommandInput
  | PublishCommandInput
  | RampExperimentCommandInput
  | RemoveMembersFromGroupCommandInput
  | ResumeExperimentCommandInput
  | TestCommandInput
  | UpdateDefaultConfigCommandInput
  | UpdateDimensionCommandInput
  | UpdateExperimentGroupCommandInput
  | UpdateFunctionCommandInput
  | UpdateOrganisationCommandInput
  | UpdateOverrideCommandInput
  | UpdateOverridesExperimentCommandInput
  | UpdateTypeTemplatesCommandInput
  | UpdateWebhookCommandInput
  | UpdateWorkspaceCommandInput
  | WeightRecomputeCommandInput;

/**
 * @public
 */
export type ServiceOutputTypes =
  | AddMembersToGroupCommandOutput
  | ApplicableVariantsCommandOutput
  | BulkOperationCommandOutput
  | ConcludeExperimentCommandOutput
  | CreateContextCommandOutput
  | CreateDefaultConfigCommandOutput
  | CreateDimensionCommandOutput
  | CreateExperimentCommandOutput
  | CreateExperimentGroupCommandOutput
  | CreateFunctionCommandOutput
  | CreateOrganisationCommandOutput
  | CreateTypeTemplatesCommandOutput
  | CreateWebhookCommandOutput
  | CreateWorkspaceCommandOutput
  | DeleteContextCommandOutput
  | DeleteDefaultConfigCommandOutput
  | DeleteDimensionCommandOutput
  | DeleteExperimentGroupCommandOutput
  | DeleteFunctionCommandOutput
  | DeleteTypeTemplatesCommandOutput
  | DiscardExperimentCommandOutput
  | GetConfigCommandOutput
  | GetConfigFastCommandOutput
  | GetContextCommandOutput
  | GetContextFromConditionCommandOutput
  | GetDimensionCommandOutput
  | GetExperimentCommandOutput
  | GetExperimentGroupCommandOutput
  | GetFunctionCommandOutput
  | GetOrganisationCommandOutput
  | GetResolvedConfigCommandOutput
  | GetTypeTemplatesListCommandOutput
  | GetWebhookCommandOutput
  | ListAuditLogsCommandOutput
  | ListContextsCommandOutput
  | ListDefaultConfigsCommandOutput
  | ListDimensionsCommandOutput
  | ListExperimentCommandOutput
  | ListExperimentGroupsCommandOutput
  | ListFunctionCommandOutput
  | ListOrganisationCommandOutput
  | ListVersionsCommandOutput
  | ListWebhookCommandOutput
  | ListWorkspaceCommandOutput
  | MigrateWorkspaceSchemaCommandOutput
  | MoveContextCommandOutput
  | PauseExperimentCommandOutput
  | PublishCommandOutput
  | RampExperimentCommandOutput
  | RemoveMembersFromGroupCommandOutput
  | ResumeExperimentCommandOutput
  | TestCommandOutput
  | UpdateDefaultConfigCommandOutput
  | UpdateDimensionCommandOutput
  | UpdateExperimentGroupCommandOutput
  | UpdateFunctionCommandOutput
  | UpdateOrganisationCommandOutput
  | UpdateOverrideCommandOutput
  | UpdateOverridesExperimentCommandOutput
  | UpdateTypeTemplatesCommandOutput
  | UpdateWebhookCommandOutput
  | UpdateWorkspaceCommandOutput
  | WeightRecomputeCommandOutput;

/**
 * @public
 */
export interface ClientDefaults
  extends Partial<__SmithyConfiguration<__HttpHandlerOptions>> {
  /**
   * The HTTP handler to use or its constructor options. Fetch in browser and Https in Nodejs.
   */
  requestHandler?: __HttpHandlerUserInput;

  /**
   * A constructor for a class implementing the {@link @smithy/types#ChecksumConstructor} interface
   * that computes the SHA-256 HMAC or checksum of a string or binary buffer.
   * @internal
   */
  sha256?: __ChecksumConstructor | __HashConstructor;

  /**
   * The function that will be used to convert strings into HTTP endpoints.
   * @internal
   */
  urlParser?: __UrlParser;

  /**
   * A function that can calculate the length of a request body.
   * @internal
   */
  bodyLengthChecker?: __BodyLengthCalculator;

  /**
   * A function that converts a stream into an array of bytes.
   * @internal
   */
  streamCollector?: __StreamCollector;

  /**
   * The function that will be used to convert a base64-encoded string to a byte array.
   * @internal
   */
  base64Decoder?: __Decoder;

  /**
   * The function that will be used to convert binary data to a base64-encoded string.
   * @internal
   */
  base64Encoder?: __Encoder;

  /**
   * The function that will be used to convert a UTF8-encoded string to a byte array.
   * @internal
   */
  utf8Decoder?: __Decoder;

  /**
   * The function that will be used to convert binary data to a UTF-8 encoded string.
   * @internal
   */
  utf8Encoder?: __Encoder;

  /**
   * The runtime environment.
   * @internal
   */
  runtime?: string;

  /**
   * Disable dynamically changing the endpoint of the client based on the hostPrefix
   * trait of an operation.
   */
  disableHostPrefix?: boolean;

  /**
   * Setting a client profile is similar to setting a value for the
   * AWS_PROFILE environment variable. Setting a profile on a client
   * in code only affects the single client instance, unlike AWS_PROFILE.
   *
   * When set, and only for environments where an AWS configuration
   * file exists, fields configurable by this file will be retrieved
   * from the specified profile within that file.
   * Conflicting code configuration and environment variables will
   * still have higher priority.
   *
   * For client credential resolution that involves checking the AWS
   * configuration file, the client's profile (this value) will be
   * used unless a different profile is set in the credential
   * provider options.
   *
   */
  profile?: string;

  /**
   * The provider populating default tracking information to be sent with `user-agent`, `x-amz-user-agent` header
   * @internal
   */
  defaultUserAgentProvider?: Provider<__UserAgent>;

  /**
   * Value for how many times a request will be made at most in case of retry.
   */
  maxAttempts?: number | __Provider<number>;

  /**
   * Specifies which retry algorithm to use.
   * @see https://docs.aws.amazon.com/AWSJavaScriptSDK/v3/latest/Package/-smithy-util-retry/Enum/RETRY_MODES/
   *
   */
  retryMode?: string | __Provider<string>;

  /**
   * Optional logger for logging debug/info/warn/error.
   */
  logger?: __Logger;

  /**
   * Optional extensions
   */
  extensions?: RuntimeExtension[];

  /**
   * The {@link @smithy/smithy-client#DefaultsMode} that will be used to determine how certain default configuration options are resolved in the SDK.
   */
  defaultsMode?: __DefaultsMode | __Provider<__DefaultsMode>;

}

/**
 * @public
 */
export type SuperpositionClientConfigType = Partial<__SmithyConfiguration<__HttpHandlerOptions>>
  & ClientDefaults
  & UserAgentInputConfig
  & CustomEndpointsInputConfig
  & RetryInputConfig
  & HostHeaderInputConfig
  & HttpAuthSchemeInputConfig
/**
 * @public
 *
 *  The configuration interface of SuperpositionClient class constructor that set the region, credentials and other options.
 */
export interface SuperpositionClientConfig extends SuperpositionClientConfigType {}

/**
 * @public
 */
export type SuperpositionClientResolvedConfigType = __SmithyResolvedConfiguration<__HttpHandlerOptions>
  & Required<ClientDefaults>
  & RuntimeExtensionsConfig
  & UserAgentResolvedConfig
  & CustomEndpointsResolvedConfig
  & RetryResolvedConfig
  & HostHeaderResolvedConfig
  & HttpAuthSchemeResolvedConfig
/**
 * @public
 *
 *  The resolved configuration interface of SuperpositionClient class. This is resolved and normalized from the {@link SuperpositionClientConfig | constructor configuration interface}.
 */
export interface SuperpositionClientResolvedConfig extends SuperpositionClientResolvedConfigType {}

/**
 * @public
 */
export class SuperpositionClient extends __Client<
  __HttpHandlerOptions,
  ServiceInputTypes,
  ServiceOutputTypes,
  SuperpositionClientResolvedConfig
> {
  /**
   * The resolved configuration of SuperpositionClient class. This is resolved and normalized from the {@link SuperpositionClientConfig | constructor configuration interface}.
   */
  readonly config: SuperpositionClientResolvedConfig;

  constructor(...[configuration]: __CheckOptionalClientConfig<SuperpositionClientConfig>) {
    let _config_0 = __getRuntimeConfig(configuration || {});
    let _config_1 = resolveUserAgentConfig(_config_0);
    let _config_2 = resolveCustomEndpointsConfig(_config_1);
    let _config_3 = resolveRetryConfig(_config_2);
    let _config_4 = resolveHostHeaderConfig(_config_3);
    let _config_5 = resolveHttpAuthSchemeConfig(_config_4);
    let _config_6 = resolveRuntimeExtensions(_config_5, configuration?.extensions || []);
    super(_config_6);
    this.config = _config_6;
    this.middlewareStack.use(getUserAgentPlugin(this.config
    ));
    this.middlewareStack.use(getRetryPlugin(this.config
    ));
    this.middlewareStack.use(getContentLengthPlugin(this.config
    ));
    this.middlewareStack.use(getHostHeaderPlugin(this.config
    ));
    this.middlewareStack.use(getLoggerPlugin(this.config
    ));
    this.middlewareStack.use(getRecursionDetectionPlugin(this.config
    ));
    this.middlewareStack.use(getHttpAuthSchemePlugin(this.config
      , {
        httpAuthSchemeParametersProvider: defaultSuperpositionHttpAuthSchemeParametersProvider,identityProviderConfigProvider: async (config: SuperpositionClientResolvedConfig) => new DefaultIdentityProviderConfig({
          "smithy.api#httpBearerAuth": config.token,}), }
    ));
    this.middlewareStack.use(getHttpSigningPlugin(this.config
    ));
  }

  /**
   * Destroy underlying resources, like sockets. It's usually not necessary to do this.
   * However in Node.js, it's best to explicitly shut down the client's agent when it is no longer needed.
   * Otherwise, sockets might stay open for quite a long time before the server terminates them.
   */
  destroy(): void {
    super.destroy();
  }
}
