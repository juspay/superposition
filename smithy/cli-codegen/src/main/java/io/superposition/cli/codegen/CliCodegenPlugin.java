package io.superposition.cli.codegen;

import software.amazon.smithy.build.PluginContext;
import software.amazon.smithy.build.SmithyBuildPlugin;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.node.ObjectNode;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;

/**
 * Smithy build plugin that generates a Rust CLI application (clap-based)
 * for the Superposition API. Each resource becomes a subcommand group and
 * each operation becomes a subcommand with typed arguments.
 */
public final class CliCodegenPlugin implements SmithyBuildPlugin {

    @Override
    public String getName() {
        return "cli-codegen";
    }

    @Override
    public void execute(PluginContext context) {
        ObjectNode settings = context.getSettings();
        Model model = context.getModel();

        String serviceId = settings.getStringMemberOrDefault(
                "service", "io.superposition#Superposition");
        String outputCrate = settings.getStringMemberOrDefault(
                "outputCrate", "superposition_cli");
        String sdkCrate = settings.getStringMemberOrDefault(
                "sdkCrate", "superposition_sdk");
        String binaryName = settings.getStringMemberOrDefault(
                "binaryName", "superposition");

        ServiceShape service = model.expectShape(
                ShapeId.from(serviceId), ServiceShape.class);

        CliCodegenConfig config = new CliCodegenConfig(
                service, outputCrate, sdkCrate, binaryName);

        RustCliGenerator generator = new RustCliGenerator(model, config);
        generator.generate(context.getFileManifest());
    }
}
