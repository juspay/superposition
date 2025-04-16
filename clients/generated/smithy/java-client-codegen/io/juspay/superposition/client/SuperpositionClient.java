
package io.juspay.superposition.client;

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
import io.juspay.superposition.model.CreateExperimentInput;
import io.juspay.superposition.model.CreateExperimentOutput;
import io.juspay.superposition.model.CreateFunctionInput;
import io.juspay.superposition.model.CreateFunctionOutput;
import io.juspay.superposition.model.CreateOrganisationInput;
import io.juspay.superposition.model.CreateOrganisationOutput;
import io.juspay.superposition.model.CreateTypeTemplatesInput;
import io.juspay.superposition.model.CreateTypeTemplatesOutput;
import io.juspay.superposition.model.CreateWorkspaceInput;
import io.juspay.superposition.model.CreateWorkspaceOutput;
import io.juspay.superposition.model.DeleteContextInput;
import io.juspay.superposition.model.DeleteContextOutput;
import io.juspay.superposition.model.DeleteDefaultConfigInput;
import io.juspay.superposition.model.DeleteDefaultConfigOutput;
import io.juspay.superposition.model.DeleteDimensionInput;
import io.juspay.superposition.model.DeleteDimensionOutput;
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
import io.juspay.superposition.model.InternalServerError;
import io.juspay.superposition.model.ListAuditLogsInput;
import io.juspay.superposition.model.ListAuditLogsOutput;
import io.juspay.superposition.model.ListContextsInput;
import io.juspay.superposition.model.ListContextsOutput;
import io.juspay.superposition.model.ListDefaultConfigsInput;
import io.juspay.superposition.model.ListDefaultConfigsOutput;
import io.juspay.superposition.model.ListDimensionsInput;
import io.juspay.superposition.model.ListDimensionsOutput;
import io.juspay.superposition.model.ListExperimentInput;
import io.juspay.superposition.model.ListExperimentOutput;
import io.juspay.superposition.model.ListFunctionInput;
import io.juspay.superposition.model.ListFunctionOutput;
import io.juspay.superposition.model.ListOrganisationInput;
import io.juspay.superposition.model.ListOrganisationOutput;
import io.juspay.superposition.model.ListVersionsInput;
import io.juspay.superposition.model.ListVersionsOutput;
import io.juspay.superposition.model.ListWorkspaceInput;
import io.juspay.superposition.model.ListWorkspaceOutput;
import io.juspay.superposition.model.MoveContextInput;
import io.juspay.superposition.model.MoveContextOutput;
import io.juspay.superposition.model.OrganisationNotFound;
import io.juspay.superposition.model.PublishInput;
import io.juspay.superposition.model.PublishOutput;
import io.juspay.superposition.model.RampExperimentInput;
import io.juspay.superposition.model.RampExperimentOutput;
import io.juspay.superposition.model.ResourceNotFound;
import io.juspay.superposition.model.TestInput;
import io.juspay.superposition.model.TestOutput;
import io.juspay.superposition.model.TypeTemplatesNotFound;
import io.juspay.superposition.model.UpdateDefaultConfigInput;
import io.juspay.superposition.model.UpdateDefaultConfigOutput;
import io.juspay.superposition.model.UpdateDimensionInput;
import io.juspay.superposition.model.UpdateDimensionOutput;
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
import io.juspay.superposition.model.UpdateWorkspaceInput;
import io.juspay.superposition.model.UpdateWorkspaceOutput;
import io.juspay.superposition.model.WeightRecomputeInput;
import io.juspay.superposition.model.WeightRecomputeOutput;
import io.juspay.superposition.model.WorkspaceNotFound;
import software.amazon.smithy.java.client.core.Client;
import software.amazon.smithy.java.client.core.ClientConfig;
import software.amazon.smithy.java.client.core.RequestOverrideConfig;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public interface SuperpositionClient {

    /**
     * @throws InternalServerError
     */
    default ApplicableVariantsOutput applicableVariants(ApplicableVariantsInput input) {
        return applicableVariants(input, null);
    }

    /**
     * @throws InternalServerError
     */
    ApplicableVariantsOutput applicableVariants(ApplicableVariantsInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default BulkOperationOutput bulkOperation(BulkOperationInput input) {
        return bulkOperation(input, null);
    }

    /**
     * @throws InternalServerError
     */
    BulkOperationOutput bulkOperation(BulkOperationInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default ConcludeExperimentOutput concludeExperiment(ConcludeExperimentInput input) {
        return concludeExperiment(input, null);
    }

    /**
     * @throws InternalServerError
     */
    ConcludeExperimentOutput concludeExperiment(ConcludeExperimentInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default CreateContextOutput createContext(CreateContextInput input) {
        return createContext(input, null);
    }

    /**
     * @throws InternalServerError
     */
    CreateContextOutput createContext(CreateContextInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default CreateDefaultConfigOutput createDefaultConfig(CreateDefaultConfigInput input) {
        return createDefaultConfig(input, null);
    }

    /**
     * @throws InternalServerError
     */
    CreateDefaultConfigOutput createDefaultConfig(CreateDefaultConfigInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default CreateDimensionOutput createDimension(CreateDimensionInput input) {
        return createDimension(input, null);
    }

    /**
     * @throws InternalServerError
     */
    CreateDimensionOutput createDimension(CreateDimensionInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default CreateExperimentOutput createExperiment(CreateExperimentInput input) {
        return createExperiment(input, null);
    }

    /**
     * @throws InternalServerError
     */
    CreateExperimentOutput createExperiment(CreateExperimentInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default CreateFunctionOutput createFunction(CreateFunctionInput input) {
        return createFunction(input, null);
    }

    /**
     * @throws InternalServerError
     */
    CreateFunctionOutput createFunction(CreateFunctionInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default CreateOrganisationOutput createOrganisation(CreateOrganisationInput input) {
        return createOrganisation(input, null);
    }

    /**
     * @throws InternalServerError
     */
    CreateOrganisationOutput createOrganisation(CreateOrganisationInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default CreateTypeTemplatesOutput createTypeTemplates(CreateTypeTemplatesInput input) {
        return createTypeTemplates(input, null);
    }

    /**
     * @throws InternalServerError
     */
    CreateTypeTemplatesOutput createTypeTemplates(CreateTypeTemplatesInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default CreateWorkspaceOutput createWorkspace(CreateWorkspaceInput input) {
        return createWorkspace(input, null);
    }

    /**
     * @throws InternalServerError
     */
    CreateWorkspaceOutput createWorkspace(CreateWorkspaceInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default DeleteContextOutput deleteContext(DeleteContextInput input) {
        return deleteContext(input, null);
    }

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    DeleteContextOutput deleteContext(DeleteContextInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default DeleteDefaultConfigOutput deleteDefaultConfig(DeleteDefaultConfigInput input) {
        return deleteDefaultConfig(input, null);
    }

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    DeleteDefaultConfigOutput deleteDefaultConfig(DeleteDefaultConfigInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default DeleteDimensionOutput deleteDimension(DeleteDimensionInput input) {
        return deleteDimension(input, null);
    }

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    DeleteDimensionOutput deleteDimension(DeleteDimensionInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    default DeleteFunctionOutput deleteFunction(DeleteFunctionInput input) {
        return deleteFunction(input, null);
    }

    /**
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    DeleteFunctionOutput deleteFunction(DeleteFunctionInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws TypeTemplatesNotFound
     * @throws InternalServerError
     */
    default DeleteTypeTemplatesOutput deleteTypeTemplates(DeleteTypeTemplatesInput input) {
        return deleteTypeTemplates(input, null);
    }

    /**
     * @throws TypeTemplatesNotFound
     * @throws InternalServerError
     */
    DeleteTypeTemplatesOutput deleteTypeTemplates(DeleteTypeTemplatesInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default DiscardExperimentOutput discardExperiment(DiscardExperimentInput input) {
        return discardExperiment(input, null);
    }

    /**
     * @throws InternalServerError
     */
    DiscardExperimentOutput discardExperiment(DiscardExperimentInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default GetConfigOutput getConfig(GetConfigInput input) {
        return getConfig(input, null);
    }

    /**
     * @throws InternalServerError
     */
    GetConfigOutput getConfig(GetConfigInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default GetConfigFastOutput getConfigFast(GetConfigFastInput input) {
        return getConfigFast(input, null);
    }

    /**
     * @throws InternalServerError
     */
    GetConfigFastOutput getConfigFast(GetConfigFastInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default GetContextOutput getContext(GetContextInput input) {
        return getContext(input, null);
    }

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    GetContextOutput getContext(GetContextInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default GetContextFromConditionOutput getContextFromCondition(GetContextFromConditionInput input) {
        return getContextFromCondition(input, null);
    }

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    GetContextFromConditionOutput getContextFromCondition(GetContextFromConditionInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default GetExperimentOutput getExperiment(GetExperimentInput input) {
        return getExperiment(input, null);
    }

    /**
     * @throws InternalServerError
     */
    GetExperimentOutput getExperiment(GetExperimentInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    default GetFunctionOutput getFunction(GetFunctionInput input) {
        return getFunction(input, null);
    }

    /**
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    GetFunctionOutput getFunction(GetFunctionInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws OrganisationNotFound
     * @throws InternalServerError
     */
    default GetOrganisationOutput getOrganisation(GetOrganisationInput input) {
        return getOrganisation(input, null);
    }

    /**
     * @throws OrganisationNotFound
     * @throws InternalServerError
     */
    GetOrganisationOutput getOrganisation(GetOrganisationInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default GetResolvedConfigOutput getResolvedConfig(GetResolvedConfigInput input) {
        return getResolvedConfig(input, null);
    }

    /**
     * @throws InternalServerError
     */
    GetResolvedConfigOutput getResolvedConfig(GetResolvedConfigInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default GetTypeTemplatesListOutput getTypeTemplatesList(GetTypeTemplatesListInput input) {
        return getTypeTemplatesList(input, null);
    }

    /**
     * @throws InternalServerError
     */
    GetTypeTemplatesListOutput getTypeTemplatesList(GetTypeTemplatesListInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default ListAuditLogsOutput listAuditLogs(ListAuditLogsInput input) {
        return listAuditLogs(input, null);
    }

    /**
     * @throws InternalServerError
     */
    ListAuditLogsOutput listAuditLogs(ListAuditLogsInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default ListContextsOutput listContexts(ListContextsInput input) {
        return listContexts(input, null);
    }

    /**
     * @throws InternalServerError
     */
    ListContextsOutput listContexts(ListContextsInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default ListDefaultConfigsOutput listDefaultConfigs(ListDefaultConfigsInput input) {
        return listDefaultConfigs(input, null);
    }

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    ListDefaultConfigsOutput listDefaultConfigs(ListDefaultConfigsInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default ListDimensionsOutput listDimensions(ListDimensionsInput input) {
        return listDimensions(input, null);
    }

    /**
     * @throws InternalServerError
     */
    ListDimensionsOutput listDimensions(ListDimensionsInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default ListExperimentOutput listExperiment(ListExperimentInput input) {
        return listExperiment(input, null);
    }

    /**
     * @throws InternalServerError
     */
    ListExperimentOutput listExperiment(ListExperimentInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default ListFunctionOutput listFunction(ListFunctionInput input) {
        return listFunction(input, null);
    }

    /**
     * @throws InternalServerError
     */
    ListFunctionOutput listFunction(ListFunctionInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default ListOrganisationOutput listOrganisation(ListOrganisationInput input) {
        return listOrganisation(input, null);
    }

    /**
     * @throws InternalServerError
     */
    ListOrganisationOutput listOrganisation(ListOrganisationInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default ListVersionsOutput listVersions(ListVersionsInput input) {
        return listVersions(input, null);
    }

    /**
     * @throws InternalServerError
     */
    ListVersionsOutput listVersions(ListVersionsInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default ListWorkspaceOutput listWorkspace(ListWorkspaceInput input) {
        return listWorkspace(input, null);
    }

    /**
     * @throws InternalServerError
     */
    ListWorkspaceOutput listWorkspace(ListWorkspaceInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default MoveContextOutput moveContext(MoveContextInput input) {
        return moveContext(input, null);
    }

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    MoveContextOutput moveContext(MoveContextInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    default PublishOutput publish(PublishInput input) {
        return publish(input, null);
    }

    /**
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    PublishOutput publish(PublishInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default RampExperimentOutput rampExperiment(RampExperimentInput input) {
        return rampExperiment(input, null);
    }

    /**
     * @throws InternalServerError
     */
    RampExperimentOutput rampExperiment(RampExperimentInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    default TestOutput test(TestInput input) {
        return test(input, null);
    }

    /**
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    TestOutput test(TestInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default UpdateDefaultConfigOutput updateDefaultConfig(UpdateDefaultConfigInput input) {
        return updateDefaultConfig(input, null);
    }

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    UpdateDefaultConfigOutput updateDefaultConfig(UpdateDefaultConfigInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default UpdateDimensionOutput updateDimension(UpdateDimensionInput input) {
        return updateDimension(input, null);
    }

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    UpdateDimensionOutput updateDimension(UpdateDimensionInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    default UpdateFunctionOutput updateFunction(UpdateFunctionInput input) {
        return updateFunction(input, null);
    }

    /**
     * @throws FunctionNotFound
     * @throws InternalServerError
     */
    UpdateFunctionOutput updateFunction(UpdateFunctionInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws OrganisationNotFound
     * @throws InternalServerError
     */
    default UpdateOrganisationOutput updateOrganisation(UpdateOrganisationInput input) {
        return updateOrganisation(input, null);
    }

    /**
     * @throws OrganisationNotFound
     * @throws InternalServerError
     */
    UpdateOrganisationOutput updateOrganisation(UpdateOrganisationInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    default UpdateOverrideOutput updateOverride(UpdateOverrideInput input) {
        return updateOverride(input, null);
    }

    /**
     * @throws ResourceNotFound
     * @throws InternalServerError
     */
    UpdateOverrideOutput updateOverride(UpdateOverrideInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default UpdateOverridesExperimentOutput updateOverridesExperiment(UpdateOverridesExperimentInput input) {
        return updateOverridesExperiment(input, null);
    }

    /**
     * @throws InternalServerError
     */
    UpdateOverridesExperimentOutput updateOverridesExperiment(UpdateOverridesExperimentInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws TypeTemplatesNotFound
     * @throws InternalServerError
     */
    default UpdateTypeTemplatesOutput updateTypeTemplates(UpdateTypeTemplatesInput input) {
        return updateTypeTemplates(input, null);
    }

    /**
     * @throws TypeTemplatesNotFound
     * @throws InternalServerError
     */
    UpdateTypeTemplatesOutput updateTypeTemplates(UpdateTypeTemplatesInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws WorkspaceNotFound
     * @throws InternalServerError
     */
    default UpdateWorkspaceOutput updateWorkspace(UpdateWorkspaceInput input) {
        return updateWorkspace(input, null);
    }

    /**
     * @throws WorkspaceNotFound
     * @throws InternalServerError
     */
    UpdateWorkspaceOutput updateWorkspace(UpdateWorkspaceInput input, RequestOverrideConfig overrideConfig);

    /**
     * @throws InternalServerError
     */
    default WeightRecomputeOutput weightRecompute(WeightRecomputeInput input) {
        return weightRecompute(input, null);
    }

    /**
     * @throws InternalServerError
     */
    WeightRecomputeOutput weightRecompute(WeightRecomputeInput input, RequestOverrideConfig overrideConfig);

    /**
     * @return Configuration in use by client.
     */
    ClientConfig config();

    /**
     * Create a Builder for {@link SuperpositionClient}.
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
     * Builder for {@link SuperpositionClient}.
     */
    final class Builder extends Client.Builder<SuperpositionClient, Builder> {

        private Builder() {}

        @Override
        public SuperpositionClient build() {

            return new SuperpositionClientImpl(this);
        }
    }

    /**
     * Builder used to create a {@link RequestOverrideConfig} for {@link SuperpositionClient} operations.
     */
    final class RequestOverrideBuilder extends RequestOverrideConfig.OverrideBuilder<RequestOverrideBuilder> {}
}

