package io.superposition.cli.codegen;

import java.util.List;

/**
 * Holds extracted information about a single Smithy operation,
 * ready for Rust code generation.
 */
public final class OperationInfo {

    private final String operationName;
    private final String cliCommandName;
    private final String httpMethod;
    private final String httpUri;
    private final String documentation;
    private final List<ArgMapper.ClapArg> args;
    private final boolean hasPagination;
    private final boolean hasWorkspaceMixin;
    private final boolean hasOrganisationMixin;
    private final boolean hasBodyPayload;
    private final String sdkMethodName;

    public OperationInfo(
            String operationName,
            String cliCommandName,
            String httpMethod,
            String httpUri,
            String documentation,
            List<ArgMapper.ClapArg> args,
            boolean hasPagination,
            boolean hasWorkspaceMixin,
            boolean hasOrganisationMixin,
            boolean hasBodyPayload,
            String sdkMethodName) {
        this.operationName = operationName;
        this.cliCommandName = cliCommandName;
        this.httpMethod = httpMethod;
        this.httpUri = httpUri;
        this.documentation = documentation;
        this.args = args;
        this.hasPagination = hasPagination;
        this.hasWorkspaceMixin = hasWorkspaceMixin;
        this.hasOrganisationMixin = hasOrganisationMixin;
        this.hasBodyPayload = hasBodyPayload;
        this.sdkMethodName = sdkMethodName;
    }

    public String getOperationName() {
        return operationName;
    }

    public String getCliCommandName() {
        return cliCommandName;
    }

    public String getHttpMethod() {
        return httpMethod;
    }

    public String getHttpUri() {
        return httpUri;
    }

    public String getDocumentation() {
        return documentation;
    }

    public List<ArgMapper.ClapArg> getArgs() {
        return args;
    }

    public boolean hasPagination() {
        return hasPagination;
    }

    public boolean hasWorkspaceMixin() {
        return hasWorkspaceMixin;
    }

    public boolean hasOrganisationMixin() {
        return hasOrganisationMixin;
    }

    public boolean hasBodyPayload() {
        return hasBodyPayload;
    }

    public String getSdkMethodName() {
        return sdkMethodName;
    }
}
