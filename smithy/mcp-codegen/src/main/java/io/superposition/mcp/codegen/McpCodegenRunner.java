package io.superposition.mcp.codegen;

import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.ServiceShape;
import software.amazon.smithy.model.shapes.ShapeId;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Standalone runner for the MCP codegen (alternative to using the Smithy CLI).
 * Usage: java -cp <classpath> io.superposition.mcp.codegen.McpCodegenRunner <models-dir> <output-dir>
 */
public class McpCodegenRunner {

    public static void main(String[] args) throws Exception {
        if (args.length < 2) {
            System.err.println("Usage: McpCodegenRunner <models-dir> <output-dir>");
            System.exit(1);
        }

        Path modelsDir = Paths.get(args[0]);
        Path outputDir = Paths.get(args[1]);

        if (!Files.isDirectory(modelsDir)) {
            System.err.println("Models directory does not exist: " + modelsDir);
            System.exit(1);
        }

        Files.createDirectories(outputDir);

        System.out.println("Loading Smithy models from: " + modelsDir);

        // Build the model from all .smithy files
        Model.Builder modelBuilder = Model.builder();
        Model model = Model.assembler()
                .discoverModels()
                .addImport(modelsDir)
                .assemble()
                .unwrap();

        ShapeId serviceId = ShapeId.from("io.superposition#Superposition");
        ServiceShape service = model.expectShape(serviceId, ServiceShape.class);

        System.out.println("Found service: " + service.getId());
        System.out.println("Resources: " + service.getResources().size());

        McpRustGenerator generator = new McpRustGenerator(model, service);
        generator.generate(outputDir);

        System.out.println("MCP codegen complete! Output written to: " + outputDir);
    }
}
