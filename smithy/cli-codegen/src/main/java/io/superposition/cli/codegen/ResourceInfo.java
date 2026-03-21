package io.superposition.cli.codegen;

import java.util.List;

/**
 * Holds extracted information about a Smithy resource,
 * ready for Rust code generation as a CLI subcommand group.
 */
public final class ResourceInfo {

    private final String resourceName;
    private final String cliSubcommandName;
    private final String moduleName;
    private final List<OperationInfo> operations;

    public ResourceInfo(
            String resourceName,
            String cliSubcommandName,
            String moduleName,
            List<OperationInfo> operations) {
        this.resourceName = resourceName;
        this.cliSubcommandName = cliSubcommandName;
        this.moduleName = moduleName;
        this.operations = operations;
    }

    public String getResourceName() {
        return resourceName;
    }

    public String getCliSubcommandName() {
        return cliSubcommandName;
    }

    public String getModuleName() {
        return moduleName;
    }

    public List<OperationInfo> getOperations() {
        return operations;
    }
}
