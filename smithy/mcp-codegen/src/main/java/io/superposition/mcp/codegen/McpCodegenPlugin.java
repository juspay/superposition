package io.superposition.mcp.codegen;

import software.amazon.smithy.build.PluginContext;
import software.amazon.smithy.build.SmithyBuildPlugin;
import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.logging.Logger;

/**
 * Smithy build plugin that generates Rust MCP tool code from Smithy models.
 * Reads operations, input/output shapes, and traits to deterministically produce:
 * - Parameter structs (with Deserialize + JsonSchema derives)
 * - Tool implementation methods (SDK builder calls)
 * - Tool registration (#[tool_router] impl block)
 * - Response formatting macros
 */
public class McpCodegenPlugin implements SmithyBuildPlugin {

    private static final Logger LOGGER = Logger.getLogger(McpCodegenPlugin.class.getName());

    @Override
    public String getName() {
        return "mcp-rust-codegen";
    }

    @Override
    public void execute(PluginContext context) {
        Model model = context.getModel();
        ShapeId serviceId = ShapeId.from("io.superposition#Superposition");
        ServiceShape service = model.expectShape(serviceId, ServiceShape.class);

        Path outputDir = context.getFileManifest().getBaseDir();

        LOGGER.info("MCP Codegen: Generating Rust MCP tools from Smithy model");

        McpRustGenerator generator = new McpRustGenerator(model, service);
        generator.generate(outputDir);

        LOGGER.info("MCP Codegen: Generation complete");
    }
}
