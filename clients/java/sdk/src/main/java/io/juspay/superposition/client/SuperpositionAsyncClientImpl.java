
package io.juspay.superposition.client;

import io.juspay.superposition.model.AddMembersToGroup;
import io.juspay.superposition.model.AddMembersToGroupInput;
import io.juspay.superposition.model.AddMembersToGroupOutput;
import io.juspay.superposition.model.ApplicableVariants;
import io.juspay.superposition.model.ApplicableVariantsInput;
import io.juspay.superposition.model.ApplicableVariantsOutput;
import io.juspay.superposition.model.BulkOperation;
import io.juspay.superposition.model.BulkOperationInput;
import io.juspay.superposition.model.BulkOperationOutput;
import io.juspay.superposition.model.ConcludeExperiment;
import io.juspay.superposition.model.ConcludeExperimentInput;
import io.juspay.superposition.model.ConcludeExperimentOutput;
import io.juspay.superposition.model.CreateContext;
import io.juspay.superposition.model.CreateContextInput;
import io.juspay.superposition.model.CreateContextOutput;
import io.juspay.superposition.model.CreateDefaultConfig;
import io.juspay.superposition.model.CreateDefaultConfigInput;
import io.juspay.superposition.model.CreateDefaultConfigOutput;
import io.juspay.superposition.model.CreateDimension;
import io.juspay.superposition.model.CreateDimensionInput;
import io.juspay.superposition.model.CreateDimensionOutput;
import io.juspay.superposition.model.CreateExperiment;
import io.juspay.superposition.model.CreateExperimentGroup;
import io.juspay.superposition.model.CreateExperimentGroupInput;
import io.juspay.superposition.model.CreateExperimentGroupOutput;
import io.juspay.superposition.model.CreateExperimentInput;
import io.juspay.superposition.model.CreateExperimentOutput;
import io.juspay.superposition.model.CreateFunction;
import io.juspay.superposition.model.CreateFunctionInput;
import io.juspay.superposition.model.CreateFunctionOutput;
import io.juspay.superposition.model.CreateOrganisation;
import io.juspay.superposition.model.CreateOrganisationInput;
import io.juspay.superposition.model.CreateOrganisationOutput;
import io.juspay.superposition.model.CreateTypeTemplates;
import io.juspay.superposition.model.CreateTypeTemplatesInput;
import io.juspay.superposition.model.CreateTypeTemplatesOutput;
import io.juspay.superposition.model.CreateWebhook;
import io.juspay.superposition.model.CreateWebhookInput;
import io.juspay.superposition.model.CreateWebhookOutput;
import io.juspay.superposition.model.CreateWorkspace;
import io.juspay.superposition.model.CreateWorkspaceInput;
import io.juspay.superposition.model.CreateWorkspaceOutput;
import io.juspay.superposition.model.DeleteContext;
import io.juspay.superposition.model.DeleteContextInput;
import io.juspay.superposition.model.DeleteContextOutput;
import io.juspay.superposition.model.DeleteDefaultConfig;
import io.juspay.superposition.model.DeleteDefaultConfigInput;
import io.juspay.superposition.model.DeleteDefaultConfigOutput;
import io.juspay.superposition.model.DeleteDimension;
import io.juspay.superposition.model.DeleteDimensionInput;
import io.juspay.superposition.model.DeleteDimensionOutput;
import io.juspay.superposition.model.DeleteExperimentGroup;
import io.juspay.superposition.model.DeleteExperimentGroupInput;
import io.juspay.superposition.model.DeleteExperimentGroupOutput;
import io.juspay.superposition.model.DeleteFunction;
import io.juspay.superposition.model.DeleteFunctionInput;
import io.juspay.superposition.model.DeleteFunctionOutput;
import io.juspay.superposition.model.DeleteTypeTemplates;
import io.juspay.superposition.model.DeleteTypeTemplatesInput;
import io.juspay.superposition.model.DeleteTypeTemplatesOutput;
import io.juspay.superposition.model.DiscardExperiment;
import io.juspay.superposition.model.DiscardExperimentInput;
import io.juspay.superposition.model.DiscardExperimentOutput;
import io.juspay.superposition.model.GetConfig;
import io.juspay.superposition.model.GetConfigFast;
import io.juspay.superposition.model.GetConfigFastInput;
import io.juspay.superposition.model.GetConfigFastOutput;
import io.juspay.superposition.model.GetConfigInput;
import io.juspay.superposition.model.GetConfigOutput;
import io.juspay.superposition.model.GetContext;
import io.juspay.superposition.model.GetContextFromCondition;
import io.juspay.superposition.model.GetContextFromConditionInput;
import io.juspay.superposition.model.GetContextFromConditionOutput;
import io.juspay.superposition.model.GetContextInput;
import io.juspay.superposition.model.GetContextOutput;
import io.juspay.superposition.model.GetDimension;
import io.juspay.superposition.model.GetDimensionInput;
import io.juspay.superposition.model.GetDimensionOutput;
import io.juspay.superposition.model.GetExperiment;
import io.juspay.superposition.model.GetExperimentGroup;
import io.juspay.superposition.model.GetExperimentGroupInput;
import io.juspay.superposition.model.GetExperimentGroupOutput;
import io.juspay.superposition.model.GetExperimentInput;
import io.juspay.superposition.model.GetExperimentOutput;
import io.juspay.superposition.model.GetFunction;
import io.juspay.superposition.model.GetFunctionInput;
import io.juspay.superposition.model.GetFunctionOutput;
import io.juspay.superposition.model.GetOrganisation;
import io.juspay.superposition.model.GetOrganisationInput;
import io.juspay.superposition.model.GetOrganisationOutput;
import io.juspay.superposition.model.GetResolvedConfig;
import io.juspay.superposition.model.GetResolvedConfigInput;
import io.juspay.superposition.model.GetResolvedConfigOutput;
import io.juspay.superposition.model.GetTypeTemplatesList;
import io.juspay.superposition.model.GetTypeTemplatesListInput;
import io.juspay.superposition.model.GetTypeTemplatesListOutput;
import io.juspay.superposition.model.GetWebhook;
import io.juspay.superposition.model.GetWebhookInput;
import io.juspay.superposition.model.GetWebhookOutput;
import io.juspay.superposition.model.ListAuditLogs;
import io.juspay.superposition.model.ListAuditLogsInput;
import io.juspay.superposition.model.ListAuditLogsOutput;
import io.juspay.superposition.model.ListContexts;
import io.juspay.superposition.model.ListContextsInput;
import io.juspay.superposition.model.ListContextsOutput;
import io.juspay.superposition.model.ListDefaultConfigs;
import io.juspay.superposition.model.ListDefaultConfigsInput;
import io.juspay.superposition.model.ListDefaultConfigsOutput;
import io.juspay.superposition.model.ListDimensions;
import io.juspay.superposition.model.ListDimensionsInput;
import io.juspay.superposition.model.ListDimensionsOutput;
import io.juspay.superposition.model.ListExperiment;
import io.juspay.superposition.model.ListExperimentGroups;
import io.juspay.superposition.model.ListExperimentGroupsInput;
import io.juspay.superposition.model.ListExperimentGroupsOutput;
import io.juspay.superposition.model.ListExperimentInput;
import io.juspay.superposition.model.ListExperimentOutput;
import io.juspay.superposition.model.ListFunction;
import io.juspay.superposition.model.ListFunctionInput;
import io.juspay.superposition.model.ListFunctionOutput;
import io.juspay.superposition.model.ListOrganisation;
import io.juspay.superposition.model.ListOrganisationInput;
import io.juspay.superposition.model.ListOrganisationOutput;
import io.juspay.superposition.model.ListVersions;
import io.juspay.superposition.model.ListVersionsInput;
import io.juspay.superposition.model.ListVersionsOutput;
import io.juspay.superposition.model.ListWebhook;
import io.juspay.superposition.model.ListWebhookInput;
import io.juspay.superposition.model.ListWebhookOutput;
import io.juspay.superposition.model.ListWorkspace;
import io.juspay.superposition.model.ListWorkspaceInput;
import io.juspay.superposition.model.ListWorkspaceOutput;
import io.juspay.superposition.model.MigrateWorkspaceSchema;
import io.juspay.superposition.model.MigrateWorkspaceSchemaInput;
import io.juspay.superposition.model.MigrateWorkspaceSchemaOutput;
import io.juspay.superposition.model.MoveContext;
import io.juspay.superposition.model.MoveContextInput;
import io.juspay.superposition.model.MoveContextOutput;
import io.juspay.superposition.model.PauseExperiment;
import io.juspay.superposition.model.PauseExperimentInput;
import io.juspay.superposition.model.PauseExperimentOutput;
import io.juspay.superposition.model.Publish;
import io.juspay.superposition.model.PublishInput;
import io.juspay.superposition.model.PublishOutput;
import io.juspay.superposition.model.RampExperiment;
import io.juspay.superposition.model.RampExperimentInput;
import io.juspay.superposition.model.RampExperimentOutput;
import io.juspay.superposition.model.RemoveMembersFromGroup;
import io.juspay.superposition.model.RemoveMembersFromGroupInput;
import io.juspay.superposition.model.RemoveMembersFromGroupOutput;
import io.juspay.superposition.model.ResumeExperiment;
import io.juspay.superposition.model.ResumeExperimentInput;
import io.juspay.superposition.model.ResumeExperimentOutput;
import io.juspay.superposition.model.Test;
import io.juspay.superposition.model.TestInput;
import io.juspay.superposition.model.TestOutput;
import io.juspay.superposition.model.UpdateDefaultConfig;
import io.juspay.superposition.model.UpdateDefaultConfigInput;
import io.juspay.superposition.model.UpdateDefaultConfigOutput;
import io.juspay.superposition.model.UpdateDimension;
import io.juspay.superposition.model.UpdateDimensionInput;
import io.juspay.superposition.model.UpdateDimensionOutput;
import io.juspay.superposition.model.UpdateExperimentGroup;
import io.juspay.superposition.model.UpdateExperimentGroupInput;
import io.juspay.superposition.model.UpdateExperimentGroupOutput;
import io.juspay.superposition.model.UpdateFunction;
import io.juspay.superposition.model.UpdateFunctionInput;
import io.juspay.superposition.model.UpdateFunctionOutput;
import io.juspay.superposition.model.UpdateOrganisation;
import io.juspay.superposition.model.UpdateOrganisationInput;
import io.juspay.superposition.model.UpdateOrganisationOutput;
import io.juspay.superposition.model.UpdateOverride;
import io.juspay.superposition.model.UpdateOverrideInput;
import io.juspay.superposition.model.UpdateOverrideOutput;
import io.juspay.superposition.model.UpdateOverridesExperiment;
import io.juspay.superposition.model.UpdateOverridesExperimentInput;
import io.juspay.superposition.model.UpdateOverridesExperimentOutput;
import io.juspay.superposition.model.UpdateTypeTemplates;
import io.juspay.superposition.model.UpdateTypeTemplatesInput;
import io.juspay.superposition.model.UpdateTypeTemplatesOutput;
import io.juspay.superposition.model.UpdateWebhook;
import io.juspay.superposition.model.UpdateWebhookInput;
import io.juspay.superposition.model.UpdateWebhookOutput;
import io.juspay.superposition.model.UpdateWorkspace;
import io.juspay.superposition.model.UpdateWorkspaceInput;
import io.juspay.superposition.model.UpdateWorkspaceOutput;
import io.juspay.superposition.model.WeightRecompute;
import io.juspay.superposition.model.WeightRecomputeInput;
import io.juspay.superposition.model.WeightRecomputeOutput;
import java.util.concurrent.CompletableFuture;
import software.amazon.smithy.java.client.core.Client;
import software.amazon.smithy.java.client.core.RequestOverrideConfig;
import software.amazon.smithy.java.core.serde.TypeRegistry;
import software.amazon.smithy.java.framework.model.AccessDeniedException;
import software.amazon.smithy.java.framework.model.InternalFailureException;
import software.amazon.smithy.java.framework.model.MalformedRequestException;
import software.amazon.smithy.java.framework.model.NotAuthorizedException;
import software.amazon.smithy.java.framework.model.ThrottlingException;
import software.amazon.smithy.java.framework.model.UnknownOperationException;
import software.amazon.smithy.java.framework.model.ValidationException;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
final class SuperpositionAsyncClientImpl extends Client implements SuperpositionAsyncClient {
    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(AccessDeniedException.$ID, AccessDeniedException.class, AccessDeniedException::builder)
        .putType(ValidationException.$ID, ValidationException.class, ValidationException::builder)
        .putType(NotAuthorizedException.$ID, NotAuthorizedException.class, NotAuthorizedException::builder)
        .putType(InternalFailureException.$ID, InternalFailureException.class, InternalFailureException::builder)
        .putType(UnknownOperationException.$ID, UnknownOperationException.class, UnknownOperationException::builder)
        .putType(MalformedRequestException.$ID, MalformedRequestException.class, MalformedRequestException::builder)
        .putType(ThrottlingException.$ID, ThrottlingException.class, ThrottlingException::builder)
        .build();

    SuperpositionAsyncClientImpl(SuperpositionAsyncClient.Builder builder) {
        super(builder);
    }

    @Override
    public CompletableFuture<AddMembersToGroupOutput> addMembersToGroup(AddMembersToGroupInput input, RequestOverrideConfig overrideConfig) {return call(input, AddMembersToGroup.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<ApplicableVariantsOutput> applicableVariants(ApplicableVariantsInput input, RequestOverrideConfig overrideConfig) {return call(input, ApplicableVariants.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<BulkOperationOutput> bulkOperation(BulkOperationInput input, RequestOverrideConfig overrideConfig) {return call(input, BulkOperation.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<ConcludeExperimentOutput> concludeExperiment(ConcludeExperimentInput input, RequestOverrideConfig overrideConfig) {return call(input, ConcludeExperiment.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<CreateContextOutput> createContext(CreateContextInput input, RequestOverrideConfig overrideConfig) {return call(input, CreateContext.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<CreateDefaultConfigOutput> createDefaultConfig(CreateDefaultConfigInput input, RequestOverrideConfig overrideConfig) {return call(input, CreateDefaultConfig.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<CreateDimensionOutput> createDimension(CreateDimensionInput input, RequestOverrideConfig overrideConfig) {return call(input, CreateDimension.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<CreateExperimentOutput> createExperiment(CreateExperimentInput input, RequestOverrideConfig overrideConfig) {return call(input, CreateExperiment.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<CreateExperimentGroupOutput> createExperimentGroup(CreateExperimentGroupInput input, RequestOverrideConfig overrideConfig) {return call(input, CreateExperimentGroup.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<CreateFunctionOutput> createFunction(CreateFunctionInput input, RequestOverrideConfig overrideConfig) {return call(input, CreateFunction.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<CreateOrganisationOutput> createOrganisation(CreateOrganisationInput input, RequestOverrideConfig overrideConfig) {return call(input, CreateOrganisation.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<CreateTypeTemplatesOutput> createTypeTemplates(CreateTypeTemplatesInput input, RequestOverrideConfig overrideConfig) {return call(input, CreateTypeTemplates.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<CreateWebhookOutput> createWebhook(CreateWebhookInput input, RequestOverrideConfig overrideConfig) {return call(input, CreateWebhook.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<CreateWorkspaceOutput> createWorkspace(CreateWorkspaceInput input, RequestOverrideConfig overrideConfig) {return call(input, CreateWorkspace.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<DeleteContextOutput> deleteContext(DeleteContextInput input, RequestOverrideConfig overrideConfig) {return call(input, DeleteContext.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<DeleteDefaultConfigOutput> deleteDefaultConfig(DeleteDefaultConfigInput input, RequestOverrideConfig overrideConfig) {return call(input, DeleteDefaultConfig.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<DeleteDimensionOutput> deleteDimension(DeleteDimensionInput input, RequestOverrideConfig overrideConfig) {return call(input, DeleteDimension.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<DeleteExperimentGroupOutput> deleteExperimentGroup(DeleteExperimentGroupInput input, RequestOverrideConfig overrideConfig) {return call(input, DeleteExperimentGroup.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<DeleteFunctionOutput> deleteFunction(DeleteFunctionInput input, RequestOverrideConfig overrideConfig) {return call(input, DeleteFunction.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<DeleteTypeTemplatesOutput> deleteTypeTemplates(DeleteTypeTemplatesInput input, RequestOverrideConfig overrideConfig) {return call(input, DeleteTypeTemplates.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<DiscardExperimentOutput> discardExperiment(DiscardExperimentInput input, RequestOverrideConfig overrideConfig) {return call(input, DiscardExperiment.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<GetConfigOutput> getConfig(GetConfigInput input, RequestOverrideConfig overrideConfig) {return call(input, GetConfig.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<GetConfigFastOutput> getConfigFast(GetConfigFastInput input, RequestOverrideConfig overrideConfig) {return call(input, GetConfigFast.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<GetContextOutput> getContext(GetContextInput input, RequestOverrideConfig overrideConfig) {return call(input, GetContext.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<GetContextFromConditionOutput> getContextFromCondition(GetContextFromConditionInput input, RequestOverrideConfig overrideConfig) {return call(input, GetContextFromCondition.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<GetDimensionOutput> getDimension(GetDimensionInput input, RequestOverrideConfig overrideConfig) {return call(input, GetDimension.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<GetExperimentOutput> getExperiment(GetExperimentInput input, RequestOverrideConfig overrideConfig) {return call(input, GetExperiment.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<GetExperimentGroupOutput> getExperimentGroup(GetExperimentGroupInput input, RequestOverrideConfig overrideConfig) {return call(input, GetExperimentGroup.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<GetFunctionOutput> getFunction(GetFunctionInput input, RequestOverrideConfig overrideConfig) {return call(input, GetFunction.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<GetOrganisationOutput> getOrganisation(GetOrganisationInput input, RequestOverrideConfig overrideConfig) {return call(input, GetOrganisation.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<GetResolvedConfigOutput> getResolvedConfig(GetResolvedConfigInput input, RequestOverrideConfig overrideConfig) {return call(input, GetResolvedConfig.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<GetTypeTemplatesListOutput> getTypeTemplatesList(GetTypeTemplatesListInput input, RequestOverrideConfig overrideConfig) {return call(input, GetTypeTemplatesList.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<GetWebhookOutput> getWebhook(GetWebhookInput input, RequestOverrideConfig overrideConfig) {return call(input, GetWebhook.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<ListAuditLogsOutput> listAuditLogs(ListAuditLogsInput input, RequestOverrideConfig overrideConfig) {return call(input, ListAuditLogs.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<ListContextsOutput> listContexts(ListContextsInput input, RequestOverrideConfig overrideConfig) {return call(input, ListContexts.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<ListDefaultConfigsOutput> listDefaultConfigs(ListDefaultConfigsInput input, RequestOverrideConfig overrideConfig) {return call(input, ListDefaultConfigs.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<ListDimensionsOutput> listDimensions(ListDimensionsInput input, RequestOverrideConfig overrideConfig) {return call(input, ListDimensions.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<ListExperimentOutput> listExperiment(ListExperimentInput input, RequestOverrideConfig overrideConfig) {return call(input, ListExperiment.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<ListExperimentGroupsOutput> listExperimentGroups(ListExperimentGroupsInput input, RequestOverrideConfig overrideConfig) {return call(input, ListExperimentGroups.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<ListFunctionOutput> listFunction(ListFunctionInput input, RequestOverrideConfig overrideConfig) {return call(input, ListFunction.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<ListOrganisationOutput> listOrganisation(ListOrganisationInput input, RequestOverrideConfig overrideConfig) {return call(input, ListOrganisation.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<ListVersionsOutput> listVersions(ListVersionsInput input, RequestOverrideConfig overrideConfig) {return call(input, ListVersions.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<ListWebhookOutput> listWebhook(ListWebhookInput input, RequestOverrideConfig overrideConfig) {return call(input, ListWebhook.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<ListWorkspaceOutput> listWorkspace(ListWorkspaceInput input, RequestOverrideConfig overrideConfig) {return call(input, ListWorkspace.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<MigrateWorkspaceSchemaOutput> migrateWorkspaceSchema(MigrateWorkspaceSchemaInput input, RequestOverrideConfig overrideConfig) {return call(input, MigrateWorkspaceSchema.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<MoveContextOutput> moveContext(MoveContextInput input, RequestOverrideConfig overrideConfig) {return call(input, MoveContext.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<PauseExperimentOutput> pauseExperiment(PauseExperimentInput input, RequestOverrideConfig overrideConfig) {return call(input, PauseExperiment.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<PublishOutput> publish(PublishInput input, RequestOverrideConfig overrideConfig) {return call(input, Publish.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<RampExperimentOutput> rampExperiment(RampExperimentInput input, RequestOverrideConfig overrideConfig) {return call(input, RampExperiment.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<RemoveMembersFromGroupOutput> removeMembersFromGroup(RemoveMembersFromGroupInput input, RequestOverrideConfig overrideConfig) {return call(input, RemoveMembersFromGroup.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<ResumeExperimentOutput> resumeExperiment(ResumeExperimentInput input, RequestOverrideConfig overrideConfig) {return call(input, ResumeExperiment.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<TestOutput> test(TestInput input, RequestOverrideConfig overrideConfig) {return call(input, Test.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<UpdateDefaultConfigOutput> updateDefaultConfig(UpdateDefaultConfigInput input, RequestOverrideConfig overrideConfig) {return call(input, UpdateDefaultConfig.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<UpdateDimensionOutput> updateDimension(UpdateDimensionInput input, RequestOverrideConfig overrideConfig) {return call(input, UpdateDimension.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<UpdateExperimentGroupOutput> updateExperimentGroup(UpdateExperimentGroupInput input, RequestOverrideConfig overrideConfig) {return call(input, UpdateExperimentGroup.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<UpdateFunctionOutput> updateFunction(UpdateFunctionInput input, RequestOverrideConfig overrideConfig) {return call(input, UpdateFunction.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<UpdateOrganisationOutput> updateOrganisation(UpdateOrganisationInput input, RequestOverrideConfig overrideConfig) {return call(input, UpdateOrganisation.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<UpdateOverrideOutput> updateOverride(UpdateOverrideInput input, RequestOverrideConfig overrideConfig) {return call(input, UpdateOverride.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<UpdateOverridesExperimentOutput> updateOverridesExperiment(UpdateOverridesExperimentInput input, RequestOverrideConfig overrideConfig) {return call(input, UpdateOverridesExperiment.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<UpdateTypeTemplatesOutput> updateTypeTemplates(UpdateTypeTemplatesInput input, RequestOverrideConfig overrideConfig) {return call(input, UpdateTypeTemplates.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<UpdateWebhookOutput> updateWebhook(UpdateWebhookInput input, RequestOverrideConfig overrideConfig) {return call(input, UpdateWebhook.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<UpdateWorkspaceOutput> updateWorkspace(UpdateWorkspaceInput input, RequestOverrideConfig overrideConfig) {return call(input, UpdateWorkspace.instance(), overrideConfig);
    }

    @Override
    public CompletableFuture<WeightRecomputeOutput> weightRecompute(WeightRecomputeInput input, RequestOverrideConfig overrideConfig) {return call(input, WeightRecompute.instance(), overrideConfig);
    }

    @Override
    protected TypeRegistry typeRegistry() {
        return TYPE_REGISTRY;
    }
}

