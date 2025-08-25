
package io.juspay.superposition.client;

import io.juspay.superposition.model.AddMembersToGroupInput;
import io.juspay.superposition.model.AddMembersToGroupOutput;
import io.juspay.superposition.model.ApplicableVariantsInput;
import io.juspay.superposition.model.ApplicableVariantsOutput;
import io.juspay.superposition.model.BulkOperationInput;
import io.juspay.superposition.model.BulkOperationOutput;
import io.juspay.superposition.model.ConcludeExperimentInput;
import io.juspay.superposition.model.ConcludeExperimentOutput;
import io.juspay.superposition.model.CreateContextInput;
import io.juspay.superposition.model.CreateContextOutput;
import io.juspay.superposition.model.CreateDefaultConfigInput;
import io.juspay.superposition.model.CreateDefaultConfigOutput;
import io.juspay.superposition.model.CreateDimensionInput;
import io.juspay.superposition.model.CreateDimensionOutput;
import io.juspay.superposition.model.CreateExperimentGroupInput;
import io.juspay.superposition.model.CreateExperimentGroupOutput;
import io.juspay.superposition.model.CreateExperimentInput;
import io.juspay.superposition.model.CreateExperimentOutput;
import io.juspay.superposition.model.CreateFunctionInput;
import io.juspay.superposition.model.CreateFunctionOutput;
import io.juspay.superposition.model.CreateOrganisationInput;
import io.juspay.superposition.model.CreateOrganisationOutput;
import io.juspay.superposition.model.CreateTypeTemplatesInput;
import io.juspay.superposition.model.CreateTypeTemplatesOutput;
import io.juspay.superposition.model.CreateWebhookInput;
import io.juspay.superposition.model.CreateWebhookOutput;
import io.juspay.superposition.model.CreateWorkspaceInput;
import io.juspay.superposition.model.CreateWorkspaceOutput;
import io.juspay.superposition.model.DeleteContextInput;
import io.juspay.superposition.model.DeleteContextOutput;
import io.juspay.superposition.model.DeleteDefaultConfigInput;
import io.juspay.superposition.model.DeleteDefaultConfigOutput;
import io.juspay.superposition.model.DeleteDimensionInput;
import io.juspay.superposition.model.DeleteDimensionOutput;
import io.juspay.superposition.model.DeleteExperimentGroupInput;
import io.juspay.superposition.model.DeleteExperimentGroupOutput;
import io.juspay.superposition.model.DeleteFunctionInput;
import io.juspay.superposition.model.DeleteFunctionOutput;
import io.juspay.superposition.model.DeleteTypeTemplatesInput;
import io.juspay.superposition.model.DeleteTypeTemplatesOutput;
import io.juspay.superposition.model.DiscardExperimentInput;
import io.juspay.superposition.model.DiscardExperimentOutput;
import io.juspay.superposition.model.FunctionNotFound;
import io.juspay.superposition.model.GetConfigFastInput;
import io.juspay.superposition.model.GetConfigFastOutput;
import io.juspay.superposition.model.GetConfigInput;
import io.juspay.superposition.model.GetConfigOutput;
import io.juspay.superposition.model.GetContextFromConditionInput;
import io.juspay.superposition.model.GetContextFromConditionOutput;
import io.juspay.superposition.model.GetContextInput;
import io.juspay.superposition.model.GetContextOutput;
import io.juspay.superposition.model.GetDimensionInput;
import io.juspay.superposition.model.GetDimensionOutput;
import io.juspay.superposition.model.GetExperimentGroupInput;
import io.juspay.superposition.model.GetExperimentGroupOutput;
import io.juspay.superposition.model.GetExperimentInput;
import io.juspay.superposition.model.GetExperimentOutput;
import io.juspay.superposition.model.GetFunctionInput;
import io.juspay.superposition.model.GetFunctionOutput;
import io.juspay.superposition.model.GetOrganisationInput;
import io.juspay.superposition.model.GetOrganisationOutput;
import io.juspay.superposition.model.GetResolvedConfigInput;
import io.juspay.superposition.model.GetResolvedConfigOutput;
import io.juspay.superposition.model.GetTypeTemplatesListInput;
import io.juspay.superposition.model.GetTypeTemplatesListOutput;
import io.juspay.superposition.model.GetWebhookInput;
import io.juspay.superposition.model.GetWebhookOutput;
import io.juspay.superposition.model.InternalServerError;
import io.juspay.superposition.model.ListAuditLogsInput;
import io.juspay.superposition.model.ListAuditLogsOutput;
import io.juspay.superposition.model.ListContextsInput;
import io.juspay.superposition.model.ListContextsOutput;
import io.juspay.superposition.model.ListDefaultConfigsInput;
import io.juspay.superposition.model.ListDefaultConfigsOutput;
import io.juspay.superposition.model.ListDimensionsInput;
import io.juspay.superposition.model.ListDimensionsOutput;
import io.juspay.superposition.model.ListExperimentGroupsInput;
import io.juspay.superposition.model.ListExperimentGroupsOutput;
import io.juspay.superposition.model.ListExperimentInput;
import io.juspay.superposition.model.ListExperimentOutput;
import io.juspay.superposition.model.ListFunctionInput;
import io.juspay.superposition.model.ListFunctionOutput;
import io.juspay.superposition.model.ListOrganisationInput;
import io.juspay.superposition.model.ListOrganisationOutput;
import io.juspay.superposition.model.ListVersionsInput;
import io.juspay.superposition.model.ListVersionsOutput;
import io.juspay.superposition.model.ListWebhookInput;
import io.juspay.superposition.model.ListWebhookOutput;
import io.juspay.superposition.model.ListWorkspaceInput;
import io.juspay.superposition.model.ListWorkspaceOutput;
import io.juspay.superposition.model.MigrateWorkspaceSchemaInput;
import io.juspay.superposition.model.MigrateWorkspaceSchemaOutput;
import io.juspay.superposition.model.MoveContextInput;
import io.juspay.superposition.model.MoveContextOutput;
import io.juspay.superposition.model.OrganisationNotFound;
import io.juspay.superposition.model.PauseExperimentInput;
import io.juspay.superposition.model.PauseExperimentOutput;
import io.juspay.superposition.model.PublishInput;
import io.juspay.superposition.model.PublishOutput;
import io.juspay.superposition.model.RampExperimentInput;
import io.juspay.superposition.model.RampExperimentOutput;
import io.juspay.superposition.model.RemoveMembersFromGroupInput;
import io.juspay.superposition.model.RemoveMembersFromGroupOutput;
import io.juspay.superposition.model.ResourceNotFound;
import io.juspay.superposition.model.ResumeExperimentInput;
import io.juspay.superposition.model.ResumeExperimentOutput;
import io.juspay.superposition.model.TestInput;
import io.juspay.superposition.model.TestOutput;
import io.juspay.superposition.model.TypeTemplatesNotFound;
import io.juspay.superposition.model.UpdateDefaultConfigInput;
import io.juspay.superposition.model.UpdateDefaultConfigOutput;
import io.juspay.superposition.model.UpdateDimensionInput;
import io.juspay.superposition.model.UpdateDimensionOutput;
import io.juspay.superposition.model.UpdateExperimentGroupInput;
import io.juspay.superposition.model.UpdateExperimentGroupOutput;
import io.juspay.superposition.model.UpdateFunctionInput;
import io.juspay.superposition.model.UpdateFunctionOutput;
import io.juspay.superposition.model.UpdateOrganisationInput;
import io.juspay.superposition.model.UpdateOrganisationOutput;
import io.juspay.superposition.model.UpdateOverrideInput;
import io.juspay.superposition.model.UpdateOverrideOutput;
import io.juspay.superposition.model.UpdateOverridesExperimentInput;
import io.juspay.superposition.model.UpdateOverridesExperimentOutput;
import io.juspay.superposition.model.UpdateTypeTemplatesInput;
import io.juspay.superposition.model.UpdateTypeTemplatesOutput;
import io.juspay.superposition.model.UpdateWebhookInput;
import io.juspay.superposition.model.UpdateWebhookOutput;
import io.juspay.superposition.model.UpdateWorkspaceInput;
import io.juspay.superposition.model.UpdateWorkspaceOutput;
import io.juspay.superposition.model.WebhookNotFound;
import io.juspay.superposition.model.WeightRecomputeInput;
import io.juspay.superposition.model.WeightRecomputeOutput;
import io.juspay.superposition.model.WorkspaceNotFound;
import java.util.concurrent.CompletableFuture;
import software.amazon.smithy.aws.traits.protocols.RestJson1Trait;
import software.amazon.smithy.java.aws.client.restjson.RestJsonClientProtocol;
import software.amazon.smithy.java.client.core.Client;
import software.amazon.smithy.java.client.core.ClientConfig;
import software.amazon.smithy.java.client.core.ProtocolSettings;
import software.amazon.smithy.java.client.core.RequestOverrideConfig;
import software.amazon.smithy.java.client.core.auth.scheme.AuthSchemeFactory;
import software.amazon.smithy.java.client.http.auth.HttpBearerAuthScheme;
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.HttpBearerAuthTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public interface SuperpositionAsyncClient {

    /**
     * Adds members to an existing experiment group.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<AddMembersToGroupOutput> addMembersToGroup(AddMembersToGroupInput input) {
        return addMembersToGroup(input, null);
    }

    /**
     * Adds members to an existing experiment group.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    CompletableFuture<AddMembersToGroupOutput> addMembersToGroup(AddMembersToGroupInput input, RequestOverrideConfig overrideConfig);

    /**
     * Determines which experiment variants are applicable to a given context, used for experiment
     * evaluation and variant selection.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<ApplicableVariantsOutput> applicableVariants(ApplicableVariantsInput input) {
        return applicableVariants(input, null);
    }

    /**
     * Determines which experiment variants are applicable to a given context, used for experiment
     * evaluation and variant selection.
     *
     * @throws InternalServerError
     */
    CompletableFuture<ApplicableVariantsOutput> applicableVariants(ApplicableVariantsInput input, RequestOverrideConfig overrideConfig);

    /**
     * Executes multiple context operations (PUT, REPLACE, DELETE, MOVE) in a single atomic transaction for
     * efficient batch processing.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<BulkOperationOutput> bulkOperation(BulkOperationInput input) {
        return bulkOperation(input, null);
    }

    /**
     * Executes multiple context operations (PUT, REPLACE, DELETE, MOVE) in a single atomic transaction for
     * efficient batch processing.
     *
     * @throws InternalServerError
     */
    CompletableFuture<BulkOperationOutput> bulkOperation(BulkOperationInput input, RequestOverrideConfig overrideConfig);

    /**
     * Concludes an inprogress experiment by selecting a winning variant and transitioning the experiment
     * to a concluded state.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<ConcludeExperimentOutput> concludeExperiment(ConcludeExperimentInput input) {
        return concludeExperiment(input, null);
    }

    /**
     * Concludes an inprogress experiment by selecting a winning variant and transitioning the experiment
     * to a concluded state.
     *
     * @throws InternalServerError
     */
    CompletableFuture<ConcludeExperimentOutput> concludeExperiment(ConcludeExperimentInput input, RequestOverrideConfig overrideConfig);

    /**
     * Creates a new context with specified conditions and overrides. Contexts define conditional rules for
     * config management.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<CreateContextOutput> createContext(CreateContextInput input) {
        return createContext(input, null);
    }

    /**
     * Creates a new context with specified conditions and overrides. Contexts define conditional rules for
     * config management.
     *
     * @throws InternalServerError
     */
    CompletableFuture<CreateContextOutput> createContext(CreateContextInput input, RequestOverrideConfig overrideConfig);

    /**
     * Creates a new default config entry with specified key, value, schema, and metadata. Default configs
     * serve as fallback values when no specific context matches.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<CreateDefaultConfigOutput> createDefaultConfig(CreateDefaultConfigInput input) {
        return createDefaultConfig(input, null);
    }

    /**
     * Creates a new default config entry with specified key, value, schema, and metadata. Default configs
     * serve as fallback values when no specific context matches.
     *
     * @throws InternalServerError
     */
    CompletableFuture<CreateDefaultConfigOutput> createDefaultConfig(CreateDefaultConfigInput input, RequestOverrideConfig overrideConfig);

    /**
     * Creates a new dimension with the specified json schema. Dimensions define categorical attributes
     * used for context-based config management.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<CreateDimensionOutput> createDimension(CreateDimensionInput input) {
        return createDimension(input, null);
    }

    /**
     * Creates a new dimension with the specified json schema. Dimensions define categorical attributes
     * used for context-based config management.
     *
     * @throws InternalServerError
     */
    CompletableFuture<CreateDimensionOutput> createDimension(CreateDimensionInput input, RequestOverrideConfig overrideConfig);

    /**
     * Creates a new experiment with variants, context and conditions. You can optionally specify metrics
     * and experiment group for tracking and analysis.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<CreateExperimentOutput> createExperiment(CreateExperimentInput input) {
        return createExperiment(input, null);
    }

    /**
     * Creates a new experiment with variants, context and conditions. You can optionally specify metrics
     * and experiment group for tracking and analysis.
     *
     * @throws InternalServerError
     */
    CompletableFuture<CreateExperimentOutput> createExperiment(CreateExperimentInput input, RequestOverrideConfig overrideConfig);

    /**
     * Creates a new experiment group.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<CreateExperimentGroupOutput> createExperimentGroup(CreateExperimentGroupInput input) {
        return createExperimentGroup(input, null);
    }

    /**
     * Creates a new experiment group.
     *
     * @throws InternalServerError
     */
    CompletableFuture<CreateExperimentGroupOutput> createExperimentGroup(CreateExperimentGroupInput input, RequestOverrideConfig overrideConfig);

    /**
     * Creates a new custom function for validation or autocompletion with specified code, runtime version,
     * and function type.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<CreateFunctionOutput> createFunction(CreateFunctionInput input) {
        return createFunction(input, null);
    }

    /**
     * Creates a new custom function for validation or autocompletion with specified code, runtime version,
     * and function type.
     *
     * @throws InternalServerError
     */
    CompletableFuture<CreateFunctionOutput> createFunction(CreateFunctionInput input, RequestOverrideConfig overrideConfig);

    /**
     * Creates a new organisation with specified details including name, admin contact, and organisational
     * information.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<CreateOrganisationOutput> createOrganisation(CreateOrganisationInput input) {
        return createOrganisation(input, null);
    }

    /**
     * Creates a new organisation with specified details including name, admin contact, and organisational
     * information.
     *
     * @throws InternalServerError
     */
    CompletableFuture<CreateOrganisationOutput> createOrganisation(CreateOrganisationInput input, RequestOverrideConfig overrideConfig);

    /**
     * Creates a new type template with specified schema definition, providing reusable type definitions
     * for config validation.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<CreateTypeTemplatesOutput> createTypeTemplates(CreateTypeTemplatesInput input) {
        return createTypeTemplates(input, null);
    }

    /**
     * Creates a new type template with specified schema definition, providing reusable type definitions
     * for config validation.
     *
     * @throws InternalServerError
     */
    CompletableFuture<CreateTypeTemplatesOutput> createTypeTemplates(CreateTypeTemplatesInput input, RequestOverrideConfig overrideConfig);

    /**
     * Creates a new webhook config to receive HTTP notifications when specified events occur in the
     * system.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<CreateWebhookOutput> createWebhook(CreateWebhookInput input) {
        return createWebhook(input, null);
    }

    /**
     * Creates a new webhook config to receive HTTP notifications when specified events occur in the
     * system.
     *
     * @throws InternalServerError
     */
    CompletableFuture<CreateWebhookOutput> createWebhook(CreateWebhookInput input, RequestOverrideConfig overrideConfig);

    /**
     * Creates a new workspace within an organisation, including database schema setup and isolated
     * environment for config management with specified admin and settings.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<CreateWorkspaceOutput> createWorkspace(CreateWorkspaceInput input) {
        return createWorkspace(input, null);
    }

    /**
     * Creates a new workspace within an organisation, including database schema setup and isolated
     * environment for config management with specified admin and settings.
     *
     * @throws InternalServerError
     */
    CompletableFuture<CreateWorkspaceOutput> createWorkspace(CreateWorkspaceInput input, RequestOverrideConfig overrideConfig);

    /**
     * Permanently removes a context from the workspace. This operation cannot be undone and will affect
     * config resolution.
     *
     * @throws InternalServerError
     * @throws ResourceNotFound
     */
    default CompletableFuture<DeleteContextOutput> deleteContext(DeleteContextInput input) {
        return deleteContext(input, null);
    }

    /**
     * Permanently removes a context from the workspace. This operation cannot be undone and will affect
     * config resolution.
     *
     * @throws InternalServerError
     * @throws ResourceNotFound
     */
    CompletableFuture<DeleteContextOutput> deleteContext(DeleteContextInput input, RequestOverrideConfig overrideConfig);

    /**
     * Permanently removes a default config entry from the workspace. This operation cannot be performed if
     * it affects config resolution for contexts that rely on this fallback value.
     *
     * @throws InternalServerError
     * @throws ResourceNotFound
     */
    default CompletableFuture<DeleteDefaultConfigOutput> deleteDefaultConfig(DeleteDefaultConfigInput input) {
        return deleteDefaultConfig(input, null);
    }

    /**
     * Permanently removes a default config entry from the workspace. This operation cannot be performed if
     * it affects config resolution for contexts that rely on this fallback value.
     *
     * @throws InternalServerError
     * @throws ResourceNotFound
     */
    CompletableFuture<DeleteDefaultConfigOutput> deleteDefaultConfig(DeleteDefaultConfigInput input, RequestOverrideConfig overrideConfig);

    /**
     * Permanently removes a dimension from the workspace. This operation will fail if the dimension has
     * active dependencies or is referenced by existing configurations.
     *
     * @throws InternalServerError
     * @throws ResourceNotFound
     */
    default CompletableFuture<DeleteDimensionOutput> deleteDimension(DeleteDimensionInput input) {
        return deleteDimension(input, null);
    }

    /**
     * Permanently removes a dimension from the workspace. This operation will fail if the dimension has
     * active dependencies or is referenced by existing configurations.
     *
     * @throws InternalServerError
     * @throws ResourceNotFound
     */
    CompletableFuture<DeleteDimensionOutput> deleteDimension(DeleteDimensionInput input, RequestOverrideConfig overrideConfig);

    /**
     * Deletes an experiment group.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<DeleteExperimentGroupOutput> deleteExperimentGroup(DeleteExperimentGroupInput input) {
        return deleteExperimentGroup(input, null);
    }

    /**
     * Deletes an experiment group.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    CompletableFuture<DeleteExperimentGroupOutput> deleteExperimentGroup(DeleteExperimentGroupInput input, RequestOverrideConfig overrideConfig);

    /**
     * Permanently removes a function from the workspace, deleting both draft and published versions along
     * with all associated code. It fails if already in use
     *
     * @throws InternalServerError
     * @throws FunctionNotFound
     */
    default CompletableFuture<DeleteFunctionOutput> deleteFunction(DeleteFunctionInput input) {
        return deleteFunction(input, null);
    }

    /**
     * Permanently removes a function from the workspace, deleting both draft and published versions along
     * with all associated code. It fails if already in use
     *
     * @throws InternalServerError
     * @throws FunctionNotFound
     */
    CompletableFuture<DeleteFunctionOutput> deleteFunction(DeleteFunctionInput input, RequestOverrideConfig overrideConfig);

    /**
     * Permanently removes a type template from the workspace. No checks performed while deleting
     *
     * @throws TypeTemplatesNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<DeleteTypeTemplatesOutput> deleteTypeTemplates(DeleteTypeTemplatesInput input) {
        return deleteTypeTemplates(input, null);
    }

    /**
     * Permanently removes a type template from the workspace. No checks performed while deleting
     *
     * @throws TypeTemplatesNotFound
     * @throws InternalServerError
     */
    CompletableFuture<DeleteTypeTemplatesOutput> deleteTypeTemplates(DeleteTypeTemplatesInput input, RequestOverrideConfig overrideConfig);

    /**
     * Discards an experiment without selecting a winner, effectively canceling the experiment and removing
     * its effects.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<DiscardExperimentOutput> discardExperiment(DiscardExperimentInput input) {
        return discardExperiment(input, null);
    }

    /**
     * Discards an experiment without selecting a winner, effectively canceling the experiment and removing
     * its effects.
     *
     * @throws InternalServerError
     */
    CompletableFuture<DiscardExperimentOutput> discardExperiment(DiscardExperimentInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves config data with context evaluation, including applicable contexts, overrides, and default
     * values based on provided conditions.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<GetConfigOutput> getConfig(GetConfigInput input) {
        return getConfig(input, null);
    }

    /**
     * Retrieves config data with context evaluation, including applicable contexts, overrides, and default
     * values based on provided conditions.
     *
     * @throws InternalServerError
     */
    CompletableFuture<GetConfigOutput> getConfig(GetConfigInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves the latest config with no processing for high-performance access.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<GetConfigFastOutput> getConfigFast(GetConfigFastInput input) {
        return getConfigFast(input, null);
    }

    /**
     * Retrieves the latest config with no processing for high-performance access.
     *
     * @throws InternalServerError
     */
    CompletableFuture<GetConfigFastOutput> getConfigFast(GetConfigFastInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves detailed information about a specific context by its unique identifier, including
     * conditions, overrides, and metadata.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<GetContextOutput> getContext(GetContextInput input) {
        return getContext(input, null);
    }

    /**
     * Retrieves detailed information about a specific context by its unique identifier, including
     * conditions, overrides, and metadata.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    CompletableFuture<GetContextOutput> getContext(GetContextInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves context information by matching against provided conditions. Used to find contexts that
     * would apply to specific scenarios.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<GetContextFromConditionOutput> getContextFromCondition(GetContextFromConditionInput input) {
        return getContextFromCondition(input, null);
    }

    /**
     * Retrieves context information by matching against provided conditions. Used to find contexts that
     * would apply to specific scenarios.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    CompletableFuture<GetContextFromConditionOutput> getContextFromCondition(GetContextFromConditionInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves detailed information about a specific dimension, including its schema, dependencies, and
     * configuration metadata.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<GetDimensionOutput> getDimension(GetDimensionInput input) {
        return getDimension(input, null);
    }

    /**
     * Retrieves detailed information about a specific dimension, including its schema, dependencies, and
     * configuration metadata.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    CompletableFuture<GetDimensionOutput> getDimension(GetDimensionInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves detailed information about a specific experiment, including its config, variants, status,
     * and metrics.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<GetExperimentOutput> getExperiment(GetExperimentInput input) {
        return getExperiment(input, null);
    }

    /**
     * Retrieves detailed information about a specific experiment, including its config, variants, status,
     * and metrics.
     *
     * @throws InternalServerError
     */
    CompletableFuture<GetExperimentOutput> getExperiment(GetExperimentInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves an existing experiment group by its ID.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<GetExperimentGroupOutput> getExperimentGroup(GetExperimentGroupInput input) {
        return getExperimentGroup(input, null);
    }

    /**
     * Retrieves an existing experiment group by its ID.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    CompletableFuture<GetExperimentGroupOutput> getExperimentGroup(GetExperimentGroupInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves detailed information about a specific function including its published and draft versions,
     * code, and metadata.
     *
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<GetFunctionOutput> getFunction(GetFunctionInput input) {
        return getFunction(input, null);
    }

    /**
     * Retrieves detailed information about a specific function including its published and draft versions,
     * code, and metadata.
     *
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    CompletableFuture<GetFunctionOutput> getFunction(GetFunctionInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves detailed information about a specific organisation including its status, contact details,
     * and administrative metadata.
     *
     * @throws OrganisationNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<GetOrganisationOutput> getOrganisation(GetOrganisationInput input) {
        return getOrganisation(input, null);
    }

    /**
     * Retrieves detailed information about a specific organisation including its status, contact details,
     * and administrative metadata.
     *
     * @throws OrganisationNotFound
     * @throws InternalServerError
     */
    CompletableFuture<GetOrganisationOutput> getOrganisation(GetOrganisationInput input, RequestOverrideConfig overrideConfig);

    /**
     * Resolves and merges config values based on context conditions, applying overrides and merge
     * strategies to produce the final configuration.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<GetResolvedConfigOutput> getResolvedConfig(GetResolvedConfigInput input) {
        return getResolvedConfig(input, null);
    }

    /**
     * Resolves and merges config values based on context conditions, applying overrides and merge
     * strategies to produce the final configuration.
     *
     * @throws InternalServerError
     */
    CompletableFuture<GetResolvedConfigOutput> getResolvedConfig(GetResolvedConfigInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves a paginated list of all type templates in the workspace, including their schemas and
     * metadata for type management.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<GetTypeTemplatesListOutput> getTypeTemplatesList(GetTypeTemplatesListInput input) {
        return getTypeTemplatesList(input, null);
    }

    /**
     * Retrieves a paginated list of all type templates in the workspace, including their schemas and
     * metadata for type management.
     *
     * @throws InternalServerError
     */
    CompletableFuture<GetTypeTemplatesListOutput> getTypeTemplatesList(GetTypeTemplatesListInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves detailed information about a specific webhook config, including its events, headers, and
     * trigger history.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<GetWebhookOutput> getWebhook(GetWebhookInput input) {
        return getWebhook(input, null);
    }

    /**
     * Retrieves detailed information about a specific webhook config, including its events, headers, and
     * trigger history.
     *
     * @throws InternalServerError
     */
    CompletableFuture<GetWebhookOutput> getWebhook(GetWebhookInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves a paginated list of audit logs with support for filtering by date range, table names,
     * actions, and usernames for compliance and monitoring purposes.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<ListAuditLogsOutput> listAuditLogs(ListAuditLogsInput input) {
        return listAuditLogs(input, null);
    }

    /**
     * Retrieves a paginated list of audit logs with support for filtering by date range, table names,
     * actions, and usernames for compliance and monitoring purposes.
     *
     * @throws InternalServerError
     */
    CompletableFuture<ListAuditLogsOutput> listAuditLogs(ListAuditLogsInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves a paginated list of contexts with support for filtering by creation date, modification
     * date, weight, and other criteria.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<ListContextsOutput> listContexts(ListContextsInput input) {
        return listContexts(input, null);
    }

    /**
     * Retrieves a paginated list of contexts with support for filtering by creation date, modification
     * date, weight, and other criteria.
     *
     * @throws InternalServerError
     */
    CompletableFuture<ListContextsOutput> listContexts(ListContextsInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves a paginated list of all default config entries in the workspace, including their values,
     * schemas, and metadata.
     *
     * @throws InternalServerError
     * @throws ResourceNotFound
     */
    default CompletableFuture<ListDefaultConfigsOutput> listDefaultConfigs(ListDefaultConfigsInput input) {
        return listDefaultConfigs(input, null);
    }

    /**
     * Retrieves a paginated list of all default config entries in the workspace, including their values,
     * schemas, and metadata.
     *
     * @throws InternalServerError
     * @throws ResourceNotFound
     */
    CompletableFuture<ListDefaultConfigsOutput> listDefaultConfigs(ListDefaultConfigsInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves a paginated list of all dimensions in the workspace. Dimensions are returned with their
     * details and metadata.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<ListDimensionsOutput> listDimensions(ListDimensionsInput input) {
        return listDimensions(input, null);
    }

    /**
     * Retrieves a paginated list of all dimensions in the workspace. Dimensions are returned with their
     * details and metadata.
     *
     * @throws InternalServerError
     */
    CompletableFuture<ListDimensionsOutput> listDimensions(ListDimensionsInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves a paginated list of experiments with support for filtering by status, date range, name,
     * creator, and experiment group.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<ListExperimentOutput> listExperiment(ListExperimentInput input) {
        return listExperiment(input, null);
    }

    /**
     * Retrieves a paginated list of experiments with support for filtering by status, date range, name,
     * creator, and experiment group.
     *
     * @throws InternalServerError
     */
    CompletableFuture<ListExperimentOutput> listExperiment(ListExperimentInput input, RequestOverrideConfig overrideConfig);

    /**
     * Lists experiment groups, with support for filtering and pagination.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<ListExperimentGroupsOutput> listExperimentGroups(ListExperimentGroupsInput input) {
        return listExperimentGroups(input, null);
    }

    /**
     * Lists experiment groups, with support for filtering and pagination.
     *
     * @throws InternalServerError
     */
    CompletableFuture<ListExperimentGroupsOutput> listExperimentGroups(ListExperimentGroupsInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves a paginated list of all functions in the workspace with their basic information and
     * current status.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<ListFunctionOutput> listFunction(ListFunctionInput input) {
        return listFunction(input, null);
    }

    /**
     * Retrieves a paginated list of all functions in the workspace with their basic information and
     * current status.
     *
     * @throws InternalServerError
     */
    CompletableFuture<ListFunctionOutput> listFunction(ListFunctionInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves a paginated list of all organisations with their basic information and status details.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<ListOrganisationOutput> listOrganisation(ListOrganisationInput input) {
        return listOrganisation(input, null);
    }

    /**
     * Retrieves a paginated list of all organisations with their basic information and status details.
     *
     * @throws InternalServerError
     */
    CompletableFuture<ListOrganisationOutput> listOrganisation(ListOrganisationInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves a paginated list of config versions with their metadata, hash values, and creation
     * timestamps for audit and rollback purposes.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<ListVersionsOutput> listVersions(ListVersionsInput input) {
        return listVersions(input, null);
    }

    /**
     * Retrieves a paginated list of config versions with their metadata, hash values, and creation
     * timestamps for audit and rollback purposes.
     *
     * @throws InternalServerError
     */
    CompletableFuture<ListVersionsOutput> listVersions(ListVersionsInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves a paginated list of all webhook configs in the workspace, including their status and
     * config details.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<ListWebhookOutput> listWebhook(ListWebhookInput input) {
        return listWebhook(input, null);
    }

    /**
     * Retrieves a paginated list of all webhook configs in the workspace, including their status and
     * config details.
     *
     * @throws InternalServerError
     */
    CompletableFuture<ListWebhookOutput> listWebhook(ListWebhookInput input, RequestOverrideConfig overrideConfig);

    /**
     * Retrieves a paginated list of all workspaces with optional filtering by workspace name, including
     * their status, config details, and administrative information.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<ListWorkspaceOutput> listWorkspace(ListWorkspaceInput input) {
        return listWorkspace(input, null);
    }

    /**
     * Retrieves a paginated list of all workspaces with optional filtering by workspace name, including
     * their status, config details, and administrative information.
     *
     * @throws InternalServerError
     */
    CompletableFuture<ListWorkspaceOutput> listWorkspace(ListWorkspaceInput input, RequestOverrideConfig overrideConfig);

    /**
     * Migrates the workspace database schema to the new version of the template
     *
     * @throws InternalServerError
     */
    default CompletableFuture<MigrateWorkspaceSchemaOutput> migrateWorkspaceSchema(MigrateWorkspaceSchemaInput input) {
        return migrateWorkspaceSchema(input, null);
    }

    /**
     * Migrates the workspace database schema to the new version of the template
     *
     * @throws InternalServerError
     */
    CompletableFuture<MigrateWorkspaceSchemaOutput> migrateWorkspaceSchema(MigrateWorkspaceSchemaInput input, RequestOverrideConfig overrideConfig);

    /**
     * Updates the condition of the mentioned context, if a context with the new condition already exists,
     * it merges the override and effectively deleting the old context
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<MoveContextOutput> moveContext(MoveContextInput input) {
        return moveContext(input, null);
    }

    /**
     * Updates the condition of the mentioned context, if a context with the new condition already exists,
     * it merges the override and effectively deleting the old context
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    CompletableFuture<MoveContextOutput> moveContext(MoveContextInput input, RequestOverrideConfig overrideConfig);

    /**
     * Temporarily pauses an inprogress experiment, suspending its effects while preserving the experiment
     * config for later resumption.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<PauseExperimentOutput> pauseExperiment(PauseExperimentInput input) {
        return pauseExperiment(input, null);
    }

    /**
     * Temporarily pauses an inprogress experiment, suspending its effects while preserving the experiment
     * config for later resumption.
     *
     * @throws InternalServerError
     */
    CompletableFuture<PauseExperimentOutput> pauseExperiment(PauseExperimentInput input, RequestOverrideConfig overrideConfig);

    /**
     * Publishes the draft version of a function, making it the active version used for validation or
     * autocompletion in the system.
     *
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<PublishOutput> publish(PublishInput input) {
        return publish(input, null);
    }

    /**
     * Publishes the draft version of a function, making it the active version used for validation or
     * autocompletion in the system.
     *
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    CompletableFuture<PublishOutput> publish(PublishInput input, RequestOverrideConfig overrideConfig);

    /**
     * Adjusts the traffic percentage allocation for an in-progress experiment, allowing gradual rollout or
     * rollback of experimental features.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<RampExperimentOutput> rampExperiment(RampExperimentInput input) {
        return rampExperiment(input, null);
    }

    /**
     * Adjusts the traffic percentage allocation for an in-progress experiment, allowing gradual rollout or
     * rollback of experimental features.
     *
     * @throws InternalServerError
     */
    CompletableFuture<RampExperimentOutput> rampExperiment(RampExperimentInput input, RequestOverrideConfig overrideConfig);

    /**
     * Removes members from an existing experiment group.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<RemoveMembersFromGroupOutput> removeMembersFromGroup(RemoveMembersFromGroupInput input) {
        return removeMembersFromGroup(input, null);
    }

    /**
     * Removes members from an existing experiment group.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    CompletableFuture<RemoveMembersFromGroupOutput> removeMembersFromGroup(RemoveMembersFromGroupInput input, RequestOverrideConfig overrideConfig);

    /**
     * Resumes a previously paused experiment, restoring its in-progress state and re-enabling variant
     * evaluation.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<ResumeExperimentOutput> resumeExperiment(ResumeExperimentInput input) {
        return resumeExperiment(input, null);
    }

    /**
     * Resumes a previously paused experiment, restoring its in-progress state and re-enabling variant
     * evaluation.
     *
     * @throws InternalServerError
     */
    CompletableFuture<ResumeExperimentOutput> resumeExperiment(ResumeExperimentInput input, RequestOverrideConfig overrideConfig);

    /**
     * Executes a function in test mode with provided input parameters to validate its behavior before
     * publishing or deployment.
     *
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<TestOutput> test(TestInput input) {
        return test(input, null);
    }

    /**
     * Executes a function in test mode with provided input parameters to validate its behavior before
     * publishing or deployment.
     *
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    CompletableFuture<TestOutput> test(TestInput input, RequestOverrideConfig overrideConfig);

    /**
     * Updates an existing default config entry. Allows modification of value, schema, function mappings,
     * and description while preserving the key identifier.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<UpdateDefaultConfigOutput> updateDefaultConfig(UpdateDefaultConfigInput input) {
        return updateDefaultConfig(input, null);
    }

    /**
     * Updates an existing default config entry. Allows modification of value, schema, function mappings,
     * and description while preserving the key identifier.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    CompletableFuture<UpdateDefaultConfigOutput> updateDefaultConfig(UpdateDefaultConfigInput input, RequestOverrideConfig overrideConfig);

    /**
     * Updates an existing dimension's configuration. Allows modification of schema, position, function
     * mappings, and other properties while maintaining dependency relationships.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<UpdateDimensionOutput> updateDimension(UpdateDimensionInput input) {
        return updateDimension(input, null);
    }

    /**
     * Updates an existing dimension's configuration. Allows modification of schema, position, function
     * mappings, and other properties while maintaining dependency relationships.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    CompletableFuture<UpdateDimensionOutput> updateDimension(UpdateDimensionInput input, RequestOverrideConfig overrideConfig);

    /**
     * Updates an existing experiment group. Allows partial updates to specified fields.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<UpdateExperimentGroupOutput> updateExperimentGroup(UpdateExperimentGroupInput input) {
        return updateExperimentGroup(input, null);
    }

    /**
     * Updates an existing experiment group. Allows partial updates to specified fields.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    CompletableFuture<UpdateExperimentGroupOutput> updateExperimentGroup(UpdateExperimentGroupInput input, RequestOverrideConfig overrideConfig);

    /**
     * Updates the draft version of an existing function with new code, runtime version, or description
     * while preserving the published version.
     *
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<UpdateFunctionOutput> updateFunction(UpdateFunctionInput input) {
        return updateFunction(input, null);
    }

    /**
     * Updates the draft version of an existing function with new code, runtime version, or description
     * while preserving the published version.
     *
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    CompletableFuture<UpdateFunctionOutput> updateFunction(UpdateFunctionInput input, RequestOverrideConfig overrideConfig);

    /**
     * Updates an existing organisation's information including contact details, status, and administrative
     * properties.
     *
     * @throws OrganisationNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<UpdateOrganisationOutput> updateOrganisation(UpdateOrganisationInput input) {
        return updateOrganisation(input, null);
    }

    /**
     * Updates an existing organisation's information including contact details, status, and administrative
     * properties.
     *
     * @throws OrganisationNotFound
     * @throws InternalServerError
     */
    CompletableFuture<UpdateOrganisationOutput> updateOrganisation(UpdateOrganisationInput input, RequestOverrideConfig overrideConfig);

    /**
     * Updates the overrides for an existing context. Allows modification of override values while
     * maintaining the context's conditions.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<UpdateOverrideOutput> updateOverride(UpdateOverrideInput input) {
        return updateOverride(input, null);
    }

    /**
     * Updates the overrides for an existing context. Allows modification of override values while
     * maintaining the context's conditions.
     *
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    CompletableFuture<UpdateOverrideOutput> updateOverride(UpdateOverrideInput input, RequestOverrideConfig overrideConfig);

    /**
     * Updates the overrides for specific variants within an experiment, allowing modification of
     * experiment behavior Updates the overrides for specific variants within an experiment, allowing
     * modification of experiment behavior while it is in the created state.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<UpdateOverridesExperimentOutput> updateOverridesExperiment(UpdateOverridesExperimentInput input) {
        return updateOverridesExperiment(input, null);
    }

    /**
     * Updates the overrides for specific variants within an experiment, allowing modification of
     * experiment behavior Updates the overrides for specific variants within an experiment, allowing
     * modification of experiment behavior while it is in the created state.
     *
     * @throws InternalServerError
     */
    CompletableFuture<UpdateOverridesExperimentOutput> updateOverridesExperiment(UpdateOverridesExperimentInput input, RequestOverrideConfig overrideConfig);

    /**
     * Updates an existing type template's schema definition and metadata while preserving its identifier
     * and usage history.
     *
     * @throws TypeTemplatesNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<UpdateTypeTemplatesOutput> updateTypeTemplates(UpdateTypeTemplatesInput input) {
        return updateTypeTemplates(input, null);
    }

    /**
     * Updates an existing type template's schema definition and metadata while preserving its identifier
     * and usage history.
     *
     * @throws TypeTemplatesNotFound
     * @throws InternalServerError
     */
    CompletableFuture<UpdateTypeTemplatesOutput> updateTypeTemplates(UpdateTypeTemplatesInput input, RequestOverrideConfig overrideConfig);

    /**
     * Updates an existing webhook config, allowing modification of URL, events, headers, and other webhook
     * properties.
     *
     * @throws WebhookNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<UpdateWebhookOutput> updateWebhook(UpdateWebhookInput input) {
        return updateWebhook(input, null);
    }

    /**
     * Updates an existing webhook config, allowing modification of URL, events, headers, and other webhook
     * properties.
     *
     * @throws WebhookNotFound
     * @throws InternalServerError
     */
    CompletableFuture<UpdateWebhookOutput> updateWebhook(UpdateWebhookInput input, RequestOverrideConfig overrideConfig);

    /**
     * Updates an existing workspace configuration, allowing modification of admin settings, mandatory
     * dimensions, and workspace properties. Validates config version existence if provided.
     *
     * @throws WorkspaceNotFound
     * @throws InternalServerError
     */
    default CompletableFuture<UpdateWorkspaceOutput> updateWorkspace(UpdateWorkspaceInput input) {
        return updateWorkspace(input, null);
    }

    /**
     * Updates an existing workspace configuration, allowing modification of admin settings, mandatory
     * dimensions, and workspace properties. Validates config version existence if provided.
     *
     * @throws WorkspaceNotFound
     * @throws InternalServerError
     */
    CompletableFuture<UpdateWorkspaceOutput> updateWorkspace(UpdateWorkspaceInput input, RequestOverrideConfig overrideConfig);

    /**
     * Recalculates and updates the priority weights for all contexts in the workspace based on their
     * dimensions.
     *
     * @throws InternalServerError
     */
    default CompletableFuture<WeightRecomputeOutput> weightRecompute(WeightRecomputeInput input) {
        return weightRecompute(input, null);
    }

    /**
     * Recalculates and updates the priority weights for all contexts in the workspace based on their
     * dimensions.
     *
     * @throws InternalServerError
     */
    CompletableFuture<WeightRecomputeOutput> weightRecompute(WeightRecomputeInput input, RequestOverrideConfig overrideConfig);

    /**
     * @return Configuration in use by client.
     */
    ClientConfig config();

    /**
     * Create a Builder for {@link SuperpositionAsyncClient}.
     */
    static Builder builder() {
        return new Builder();
    }

    /**
     * Create a {@link RequestOverrideConfig} builder for this client.
     */
    static RequestOverrideBuilder requestOverrideBuilder() {
        return new RequestOverrideBuilder();
    }

    /**
     * Builder for {@link SuperpositionAsyncClient}.
     */
    final class Builder extends Client.Builder<SuperpositionAsyncClient, Builder> {
        private static final ProtocolSettings protocolSettings = ProtocolSettings.builder()
                .service(ShapeId.from("io.superposition#Superposition"))
                .build();
        private static final RestJson1Trait protocolTrait = new RestJson1Trait.Provider().createTrait(
            ShapeId.from("aws.protocols#restJson1"),
            Node.objectNode()
        );

        private static final HttpBearerAuthTrait httpBearerAuthScheme = new HttpBearerAuthTrait();
        private static final AuthSchemeFactory<HttpBearerAuthTrait> httpBearerAuthSchemeFactory = new HttpBearerAuthScheme.Factory();

        private Builder() {
            configBuilder().putSupportedAuthSchemes(httpBearerAuthSchemeFactory.createAuthScheme(httpBearerAuthScheme));
        }

        @Override
        public SuperpositionAsyncClient build() {
            if (configBuilder().protocol() == null) {
                configBuilder().protocol(new RestJsonClientProtocol.Factory().createProtocol(protocolSettings, protocolTrait));
            }

            return new SuperpositionAsyncClientImpl(this);
        }
    }

    /**
     * Builder used to create a {@link RequestOverrideConfig} for {@link SuperpositionAsyncClient} operations.
     */
    final class RequestOverrideBuilder extends RequestOverrideConfig.OverrideBuilder<RequestOverrideBuilder> {}
}

