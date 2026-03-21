package io.superposition.cli.codegen;

import software.amazon.smithy.model.shapes.ServiceShape;

/**
 * Configuration for the CLI codegen plugin, extracted from smithy-build.json settings.
 */
public final class CliCodegenConfig {

    private final ServiceShape service;
    private final String outputCrate;
    private final String sdkCrate;
    private final String binaryName;

    public CliCodegenConfig(
            ServiceShape service,
            String outputCrate,
            String sdkCrate,
            String binaryName) {
        this.service = service;
        this.outputCrate = outputCrate;
        this.sdkCrate = sdkCrate;
        this.binaryName = binaryName;
    }

    public ServiceShape getService() {
        return service;
    }

    public String getOutputCrate() {
        return outputCrate;
    }

    public String getSdkCrate() {
        return sdkCrate;
    }

    public String getBinaryName() {
        return binaryName;
    }
}
