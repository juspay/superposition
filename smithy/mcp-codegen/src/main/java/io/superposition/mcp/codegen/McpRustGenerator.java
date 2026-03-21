package io.superposition.mcp.codegen;

import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.knowledge.TopDownIndex;
import software.amazon.smithy.model.shapes.*;
import software.amazon.smithy.model.traits.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.logging.Logger;
import java.util.stream.Collectors;

/**
 * Generates Rust MCP tool code from the Smithy model.
 */
public class McpRustGenerator {

    private static final Logger LOGGER = Logger.getLogger(McpRustGenerator.class.getName());

    private final Model model;
    private final ServiceShape service;
    private final TopDownIndex topDownIndex;

    // Mixin member names to skip (handled implicitly by the MCP server)
    private static final Set<String> WORKSPACE_MIXIN_MEMBERS = Set.of(
            "workspace_id", "org_id"
    );

    // Resources that use OrganisationMixin instead of WorkspaceMixin
    private static final Set<String> ORG_LEVEL_RESOURCES = Set.of(
            "Organisation", "Workspace"
    );

    public McpRustGenerator(Model model, ServiceShape service) {
        this.model = model;
        this.service = service;
        this.topDownIndex = TopDownIndex.of(model);
    }

    public void generate(Path outputDir) {
        try {
            Path genDir = outputDir;
            Files.createDirectories(genDir);

            // Group operations by resource
            Map<String, List<OperationInfo>> resourceOps = collectOperations();

            // Generate tool files per resource
            for (var entry : resourceOps.entrySet()) {
                String resourceName = entry.getKey();
                List<OperationInfo> ops = entry.getValue();
                String fileName = toSnakeCase(resourceName) + ".rs";

                String content = generateToolFile(resourceName, ops);
                Path filePath = genDir.resolve(fileName);
                Files.writeString(filePath, content);
                LOGGER.info("Generated: " + filePath);
            }

            // Generate server_tools.rs (tool registration)
            String serverTools = generateServerTools(resourceOps);
            Files.writeString(genDir.resolve("server_tools.rs"), serverTools);

            // Generate response_macros.rs
            String macros = generateResponseMacros(resourceOps);
            Files.writeString(genDir.resolve("response_macros.rs"), macros);

            // Generate mod.rs
            String modRs = generateModRs(resourceOps);
            Files.writeString(genDir.resolve("mod.rs"), modRs);

        } catch (IOException e) {
            throw new RuntimeException("Failed to generate MCP code", e);
        }
    }

    // ========== Operation Collection ==========

    private Map<String, List<OperationInfo>> collectOperations() {
        Map<String, List<OperationInfo>> result = new LinkedHashMap<>();

        for (var resourceId : service.getResources()) {
            ResourceShape resource = model.expectShape(resourceId, ResourceShape.class);
            String resourceName = resourceId.getName();
            List<OperationInfo> ops = new ArrayList<>();

            // Collect CRUD operations
            resource.getCreate().ifPresent(id -> addOp(ops, id, resourceName, "create"));
            resource.getPut().ifPresent(id -> addOp(ops, id, resourceName, "put"));
            resource.getRead().ifPresent(id -> addOp(ops, id, resourceName, "get"));
            resource.getUpdate().ifPresent(id -> addOp(ops, id, resourceName, "update"));
            resource.getDelete().ifPresent(id -> addOp(ops, id, resourceName, "delete"));
            resource.getList().ifPresent(id -> addOp(ops, id, resourceName, "list"));

            // Collect additional operations
            for (var opId : resource.getOperations()) {
                String verb = inferVerb(opId.getName(), resourceName);
                addOp(ops, opId, resourceName, verb);
            }

            // Collect collection operations
            for (var opId : resource.getCollectionOperations()) {
                String verb = inferVerb(opId.getName(), resourceName);
                addOp(ops, opId, resourceName, verb);
            }

            if (!ops.isEmpty()) {
                result.put(resourceName, ops);
            }
        }

        return result;
    }

    private void addOp(List<OperationInfo> ops, ShapeId opId, String resourceName, String verb) {
        OperationShape op = model.expectShape(opId, OperationShape.class);
        ops.add(new OperationInfo(op, resourceName, verb));
    }

    private String inferVerb(String opName, String resourceName) {
        // Remove resource name prefix to get verb
        // e.g. "CreateVariable" -> "create", "ListExperiments" -> "list"
        String lower = opName;
        // Try to extract verb by removing resource-related suffix
        String[] prefixes = {"Create", "Get", "List", "Update", "Delete", "Put",
                "Conclude", "Discard", "Ramp", "Pause", "Resume", "Publish",
                "Test", "Validate", "Move", "Rotate", "Add", "Remove"};
        for (String prefix : prefixes) {
            if (lower.startsWith(prefix)) {
                String rest = lower.substring(prefix.length());
                if (rest.isEmpty() || rest.equals(resourceName) ||
                        rest.equals(resourceName + "s") || rest.equals("s")) {
                    return toSnakeCase(prefix);
                }
                return toSnakeCase(lower);
            }
        }
        return toSnakeCase(lower);
    }

    // ========== Tool File Generation ==========

    private String generateToolFile(String resourceName, List<OperationInfo> ops) {
        StringBuilder sb = new StringBuilder();
        sb.append("// AUTO-GENERATED by smithy mcp-codegen — DO NOT EDIT\n");
        sb.append("use rmcp::model::*;\n");
        sb.append("use schemars::JsonSchema;\n");
        sb.append("use serde::Deserialize;\n\n");
        sb.append("use crate::SuperpositionMcpServer;\n");
        sb.append("use crate::helpers::*;\n\n");

        // Generate param structs
        for (OperationInfo op : ops) {
            generateParamStruct(sb, op);
        }

        // Generate impl block
        sb.append("impl SuperpositionMcpServer {\n");
        for (OperationInfo op : ops) {
            generateImplMethod(sb, op, resourceName);
        }
        sb.append("}\n");

        return sb.toString();
    }

    private void generateParamStruct(StringBuilder sb, OperationInfo opInfo) {
        OperationShape op = opInfo.operation;
        Optional<ShapeId> inputId = op.getInput();
        if (inputId.isEmpty()) return;

        StructureShape input = model.expectShape(inputId.get(), StructureShape.class);
        List<MemberInfo> members = getInputMembers(input);

        if (members.isEmpty()) return;

        String structName = opInfo.paramStructName();

        // Doc comment from operation
        op.getTrait(DocumentationTrait.class).ifPresent(doc ->
                sb.append("/// ").append(doc.getValue().replace("\n", "\n/// ")).append("\n")
        );

        sb.append("#[derive(Debug, Deserialize, JsonSchema)]\n");
        sb.append("pub struct ").append(structName).append(" {\n");

        for (MemberInfo m : members) {
            // Doc comment
            m.member.getTrait(DocumentationTrait.class).ifPresent(doc ->
                    sb.append("    /// ").append(doc.getValue().replace("\n", "\n    /// ")).append("\n")
            );

            String rustType = toRustType(m.member, m.isRequired);
            String fieldName = toRustFieldName(m.member.getMemberName());
            sb.append("    pub ").append(fieldName).append(": ").append(rustType).append(",\n");
        }
        sb.append("}\n\n");

        // Generate any nested helper structs (e.g., VariantParam)
        generateNestedStructs(sb, opInfo, members);
    }

    private void generateNestedStructs(StringBuilder sb, OperationInfo opInfo, List<MemberInfo> members) {
        for (MemberInfo m : members) {
            Shape target = model.expectShape(m.member.getTarget());
            if (target.isListShape()) {
                ListShape list = target.asListShape().get();
                Shape memberTarget = model.expectShape(list.getMember().getTarget());
                if (memberTarget.isStructureShape()) {
                    StructureShape nested = memberTarget.asStructureShape().get();
                    String nestedName = opInfo.shortName() + toPascalCase(m.member.getMemberName()) + "Item";
                    sb.append("#[derive(Debug, Deserialize, JsonSchema)]\n");
                    sb.append("pub struct ").append(nestedName).append(" {\n");
                    for (var nm : nested.getAllMembers().values()) {
                        String rustType = toRustType(nm, nm.hasTrait(RequiredTrait.class));
                        String fieldName = toRustFieldName(nm.getMemberName());
                        sb.append("    pub ").append(fieldName).append(": ").append(rustType).append(",\n");
                    }
                    sb.append("}\n\n");
                }
            }
        }
    }

    private void generateImplMethod(StringBuilder sb, OperationInfo opInfo, String resourceName) {
        OperationShape op = opInfo.operation;
        String methodName = opInfo.implMethodName();
        String paramStruct = opInfo.paramStructName();
        boolean hasInput = op.getInput().isPresent();
        Optional<ShapeId> outputId = op.getOutput();

        List<MemberInfo> members = Collections.emptyList();
        StructureShape inputShape = null;
        if (hasInput) {
            inputShape = model.expectShape(op.getInput().get(), StructureShape.class);
            members = getInputMembers(inputShape);
        }

        // Check if this is a paginated list operation
        boolean isPaginated = opInfo.verb.equals("list") && outputId.isPresent() &&
                hasPaginatedResponse(outputId.get());

        // Check if the operation has a @httpPayload member
        MemberInfo payloadMember = null;
        List<MemberInfo> headerMembers = new ArrayList<>();
        List<MemberInfo> bodyMembers = new ArrayList<>();

        if (inputShape != null) {
            for (var rawMember : inputShape.getAllMembers().values()) {
                if (rawMember.hasTrait(HttpPayloadTrait.class)) {
                    // Find this in our filtered members
                    for (MemberInfo mi : members) {
                        // The payload itself is a separate type; we flatten its fields into params
                    }
                    payloadMember = new MemberInfo(rawMember, rawMember.hasTrait(RequiredTrait.class));
                }
            }

            for (MemberInfo mi : members) {
                if (mi.member.hasTrait(HttpHeaderTrait.class)) {
                    headerMembers.add(mi);
                } else {
                    bodyMembers.add(mi);
                }
            }
        }

        boolean isOrgLevel = ORG_LEVEL_RESOURCES.contains(resourceName);

        // Method signature
        if (members.isEmpty()) {
            sb.append("    pub async fn ").append(methodName)
                    .append("(&self) -> Result<CallToolResult, rmcp::ErrorData> {\n");
        } else {
            sb.append("    pub async fn ").append(methodName)
                    .append("(&self, args: ").append(paramStruct)
                    .append(") -> Result<CallToolResult, rmcp::ErrorData> {\n");
        }

        // Build SDK call
        String sdkMethod = toSnakeCase(op.getId().getName());
        sb.append("        let mut req = self.client.").append(sdkMethod).append("()\n");

        if (!isOrgLevel || resourceName.equals("Workspace")) {
            sb.append("            .workspace_id(&self.config.workspace_id)\n");
        }
        sb.append("            .org_id(&self.config.org_id);\n");

        // Chain required body fields
        for (MemberInfo m : bodyMembers) {
            if (!m.isRequired) continue;
            generateFieldSetter(sb, m, opInfo, false);
        }

        // Chain optional body fields
        for (MemberInfo m : bodyMembers) {
            if (m.isRequired) continue;
            generateFieldSetter(sb, m, opInfo, true);
        }

        // Chain header fields (always optional in params)
        for (MemberInfo m : headerMembers) {
            generateFieldSetter(sb, m, opInfo, true);
        }

        // Send and handle response
        sb.append("        let resp = req.send().await.map_err(mcp_err)?;\n");

        if (isPaginated) {
            String macroName = toSnakeCase(resourceName) + "_to_json";
            sb.append("        let items: Vec<serde_json::Value> = resp.data.iter().map(|r| ")
                    .append(macroName).append("!(r)).collect();\n");
            sb.append("        let result = serde_json::json!({\n");
            sb.append("            \"total_pages\": resp.total_pages,\n");
            sb.append("            \"total_items\": resp.total_items,\n");
            sb.append("            \"data\": items,\n");
            sb.append("        });\n");
            sb.append("        let json = serde_json::to_string_pretty(&result).map_err(mcp_err)?;\n");
        } else if (op.hasTrait(HttpTrait.class) &&
                op.expectTrait(HttpTrait.class).getMethod().equals("DELETE") &&
                outputId.isEmpty()) {
            sb.append("        let json = \"Deleted successfully\".to_string();\n");
        } else if (outputId.isPresent()) {
            String macroName = toSnakeCase(resourceName) + "_to_json";
            sb.append("        let json = serde_json::to_string_pretty(&")
                    .append(macroName).append("!(resp)).map_err(mcp_err)?;\n");
        } else {
            sb.append("        let json = \"Success\".to_string();\n");
        }

        sb.append("        Ok(CallToolResult::success(vec![Content::text(json)]))\n");
        sb.append("    }\n\n");
    }

    private void generateFieldSetter(StringBuilder sb, MemberInfo m, OperationInfo opInfo, boolean isOptional) {
        String fieldName = toRustFieldName(m.member.getMemberName());
        String sdkMethodName = toSnakeCase(m.member.getMemberName());
        Shape target = model.expectShape(m.member.getTarget());

        // Determine the type category
        TypeCategory cat = categorizeType(target);

        if (isOptional) {
            sb.append("        if let Some(v) = args.").append(fieldName).append(" {\n");
            switch (cat) {
                case STRING, INTEGER, BOOLEAN:
                    sb.append("            req = req.").append(sdkMethodName).append("(v);\n");
                    break;
                case DOCUMENT_MAP:
                    sb.append("            req = req.set_").append(sdkMethodName)
                            .append("(Some(json_to_doc_map(v).map_err(mcp_err)?));\n");
                    break;
                case DOCUMENT:
                    sb.append("            req = req.").append(sdkMethodName)
                            .append("(json_to_doc(v));\n");
                    break;
                case STRING_LIST:
                    sb.append("            for item in v {\n");
                    sb.append("                req = req.").append(sdkMethodName).append("(item);\n");
                    sb.append("            }\n");
                    break;
                case ENUM:
                    generateEnumConversion(sb, target, sdkMethodName, "v", "            ");
                    break;
                default:
                    sb.append("            req = req.").append(sdkMethodName).append("(v);\n");
                    break;
            }
            sb.append("        }\n");
        } else {
            switch (cat) {
                case STRING, INTEGER, BOOLEAN:
                    sb.append("        req = req.").append(sdkMethodName).append("(args.")
                            .append(fieldName).append(");\n");
                    break;
                case DOCUMENT_MAP:
                    sb.append("        req = req.set_").append(sdkMethodName)
                            .append("(Some(json_to_doc_map(args.").append(fieldName)
                            .append(").map_err(mcp_err)?));\n");
                    break;
                case DOCUMENT:
                    sb.append("        req = req.").append(sdkMethodName)
                            .append("(json_to_doc(args.").append(fieldName).append("));\n");
                    break;
                case STRING_LIST:
                    sb.append("        for item in args.").append(fieldName).append(" {\n");
                    sb.append("            req = req.").append(sdkMethodName).append("(item);\n");
                    sb.append("        }\n");
                    break;
                default:
                    sb.append("        req = req.").append(sdkMethodName).append("(args.")
                            .append(fieldName).append(");\n");
                    break;
            }
        }
    }

    private void generateEnumConversion(StringBuilder sb, Shape enumShape, String sdkMethod,
                                         String varName, String indent) {
        String enumTypeName = enumShape.getId().getName();
        String sdkEnumPath = "superposition_sdk::types::" + enumTypeName;
        if (enumShape.isEnumShape()) {
            EnumShape es = enumShape.asEnumShape().get();
            sb.append(indent).append("let parsed = match ").append(varName)
                    .append(".to_uppercase().as_str() {\n");
            for (var entry : es.getEnumValues().entrySet()) {
                String memberName = entry.getKey();
                String value = entry.getValue();
                sb.append(indent).append("    \"").append(value.toUpperCase())
                        .append("\" => ").append(sdkEnumPath).append("::")
                        .append(memberName).append(",\n");
            }
            // Default to first variant
            String firstMember = es.getEnumValues().keySet().iterator().next();
            sb.append(indent).append("    _ => ").append(sdkEnumPath).append("::")
                    .append(firstMember).append(",\n");
            sb.append(indent).append("};\n");
            sb.append(indent).append("req = req.").append(sdkMethod).append("(parsed);\n");
        } else {
            // String enum - just pass through
            sb.append(indent).append("req = req.").append(sdkMethod).append("(")
                    .append(varName).append(");\n");
        }
    }

    // ========== Server Tools Registration ==========

    private String generateServerTools(Map<String, List<OperationInfo>> resourceOps) {
        StringBuilder sb = new StringBuilder();
        sb.append("// AUTO-GENERATED by smithy mcp-codegen — DO NOT EDIT\n");
        sb.append("use rmcp::handler::server::wrapper::Parameters;\n");
        sb.append("use rmcp::model::*;\n");
        sb.append("use rmcp::{tool, tool_router};\n\n");
        sb.append("use crate::SuperpositionMcpServer;\n");
        sb.append("use crate::generated::tools::*;\n\n");

        sb.append("#[tool_router]\n");
        sb.append("impl SuperpositionMcpServer {\n");

        for (var entry : resourceOps.entrySet()) {
            String resourceName = entry.getKey();
            sb.append("    // ===== ").append(resourceName).append(" =====\n");

            for (OperationInfo op : entry.getValue()) {
                String toolName = op.toolName();
                String description = op.operation.getTrait(DocumentationTrait.class)
                        .map(DocumentationTrait::getValue)
                        .orElse("No description available.")
                        .replace("\"", "\\\"");
                // Truncate long descriptions
                if (description.length() > 200) {
                    description = description.substring(0, 197) + "...";
                }

                String methodName = op.registrationMethodName();
                String implMethod = op.implMethodName();
                String paramStruct = op.paramStructName();
                boolean hasParams = op.operation.getInput().isPresent() &&
                        !getInputMembers(model.expectShape(op.operation.getInput().get(),
                                StructureShape.class)).isEmpty();

                sb.append("    #[tool(\n");
                sb.append("        name = \"").append(toolName).append("\",\n");
                sb.append("        description = \"").append(description).append("\"\n");
                sb.append("    )]\n");

                if (hasParams) {
                    sb.append("    async fn ").append(methodName).append("(\n");
                    sb.append("        &self,\n");
                    sb.append("        Parameters(args): Parameters<").append(paramStruct).append(">,\n");
                    sb.append("    ) -> Result<CallToolResult, rmcp::ErrorData> {\n");
                    sb.append("        self.").append(implMethod).append("(args).await\n");
                } else {
                    sb.append("    async fn ").append(methodName)
                            .append("(&self) -> Result<CallToolResult, rmcp::ErrorData> {\n");
                    sb.append("        self.").append(implMethod).append("().await\n");
                }
                sb.append("    }\n\n");
            }
        }

        sb.append("}\n");
        return sb.toString();
    }

    // ========== Response Macros ==========

    private String generateResponseMacros(Map<String, List<OperationInfo>> resourceOps) {
        StringBuilder sb = new StringBuilder();
        sb.append("// AUTO-GENERATED by smithy mcp-codegen — DO NOT EDIT\n\n");

        Set<String> generatedMacros = new HashSet<>();

        for (var entry : resourceOps.entrySet()) {
            String resourceName = entry.getKey();
            String macroName = toSnakeCase(resourceName) + "_to_json";

            if (generatedMacros.contains(macroName)) continue;
            generatedMacros.add(macroName);

            // Find the primary response shape for this resource
            // Usually from the Get or Create operation
            ShapeId responseShapeId = findResponseShape(entry.getValue());
            if (responseShapeId == null) continue;

            Shape responseShape = model.expectShape(responseShapeId);
            if (!responseShape.isStructureShape()) continue;

            StructureShape resp = responseShape.asStructureShape().get();

            sb.append("macro_rules! ").append(macroName).append(" {\n");
            sb.append("    ($r:expr) => {{\n");
            sb.append("        serde_json::json!({\n");

            for (var member : resp.getAllMembers().values()) {
                String name = member.getMemberName();
                Shape target = model.expectShape(member.getTarget());
                String jsonExpr = toJsonExpression("$r", name, target, member);
                sb.append("            \"").append(name).append("\": ").append(jsonExpr).append(",\n");
            }

            sb.append("        })\n");
            sb.append("    }}\n");
            sb.append("}\n\n");
            sb.append("pub(crate) use ").append(macroName).append(";\n\n");
        }

        return sb.toString();
    }

    private ShapeId findResponseShape(List<OperationInfo> ops) {
        // Prefer Get/Read operation output, then Create, then first available
        for (String verb : List.of("get", "create", "update", "list")) {
            for (OperationInfo op : ops) {
                if (op.verb.equals(verb) && op.operation.getOutput().isPresent()) {
                    ShapeId outId = op.operation.getOutput().get();
                    StructureShape outShape = model.expectShape(outId, StructureShape.class);
                    // For list operations, skip the paginated wrapper
                    if (verb.equals("list")) continue;
                    return outId;
                }
            }
        }
        // Fallback: first operation with output
        for (OperationInfo op : ops) {
            if (op.operation.getOutput().isPresent()) {
                return op.operation.getOutput().get();
            }
        }
        return null;
    }

    private String toJsonExpression(String rootVar, String fieldName, Shape target, MemberShape member) {
        String accessor = rootVar + "." + toRustFieldName(fieldName);

        if (target.isTimestampShape()) {
            if (member.hasTrait(RequiredTrait.class)) {
                return "$crate::helpers::format_datetime(&" + accessor + ")";
            } else {
                return accessor + ".as_ref().map($crate::helpers::format_datetime)";
            }
        }

        if (isDocumentMap(target)) {
            if (member.hasTrait(RequiredTrait.class)) {
                return "$crate::helpers::doc_map_to_json(&" + accessor + ")";
            } else {
                return accessor + ".as_ref().map($crate::helpers::doc_map_to_json)";
            }
        }

        if (target.isDocumentShape()) {
            if (member.hasTrait(RequiredTrait.class)) {
                return "$crate::helpers::doc_to_json(&" + accessor + ")";
            } else {
                return accessor + ".as_ref().map($crate::helpers::doc_to_json)";
            }
        }

        if (target.isEnumShape() || target.isUnionShape()) {
            return "format!(\"{:?}\", " + accessor + ")";
        }

        // Default: direct access (works for String, Integer, Boolean, Option<T>)
        return accessor;
    }

    // ========== Mod.rs Generation ==========

    private String generateModRs(Map<String, List<OperationInfo>> resourceOps) {
        StringBuilder sb = new StringBuilder();
        sb.append("// AUTO-GENERATED by smithy mcp-codegen — DO NOT EDIT\n\n");

        sb.append("pub mod tools {\n");
        for (String resourceName : resourceOps.keySet()) {
            String modName = toSnakeCase(resourceName);
            sb.append("    mod ").append(modName).append(";\n");
            sb.append("    pub use ").append(modName).append("::*;\n");
        }
        sb.append("}\n\n");

        sb.append("#[macro_use]\n");
        sb.append("mod response_macros;\n\n");

        sb.append("mod server_tools;\n");
        sb.append("pub use server_tools::*;\n");

        return sb.toString();
    }

    // ========== Helper Methods ==========

    private List<MemberInfo> getInputMembers(StructureShape input) {
        List<MemberInfo> result = new ArrayList<>();

        for (var member : input.getAllMembers().values()) {
            String name = member.getMemberName();

            // Skip workspace/org mixin members
            if (WORKSPACE_MIXIN_MEMBERS.contains(name)) continue;

            // Check for @httpPayload — if present, flatten the payload structure
            if (member.hasTrait(HttpPayloadTrait.class)) {
                Shape payloadTarget = model.expectShape(member.getTarget());
                if (payloadTarget.isStructureShape()) {
                    StructureShape payloadStruct = payloadTarget.asStructureShape().get();
                    for (var payloadMember : payloadStruct.getAllMembers().values()) {
                        boolean required = payloadMember.hasTrait(RequiredTrait.class);
                        result.add(new MemberInfo(payloadMember, required));
                    }
                }
                continue;
            }

            boolean required = member.hasTrait(RequiredTrait.class);

            // @httpLabel members are always required
            if (member.hasTrait(HttpLabelTrait.class)) {
                required = true;
            }

            result.add(new MemberInfo(member, required));
        }

        return result;
    }

    private boolean hasPaginatedResponse(ShapeId outputId) {
        StructureShape output = model.expectShape(outputId, StructureShape.class);
        return output.getMember("total_pages").isPresent() &&
                output.getMember("data").isPresent();
    }

    private boolean isDocumentMap(Shape shape) {
        if (shape.isMapShape()) {
            MapShape map = shape.asMapShape().get();
            Shape valueTarget = model.expectShape(map.getValue().getTarget());
            return valueTarget.isDocumentShape();
        }
        return false;
    }

    enum TypeCategory {
        STRING, INTEGER, BOOLEAN, DOCUMENT, DOCUMENT_MAP, STRING_LIST, STRUCT_LIST, ENUM, OTHER
    }

    private TypeCategory categorizeType(Shape target) {
        if (target.isStringShape()) return TypeCategory.STRING;
        if (target.isIntegerShape() || target.isLongShape()) return TypeCategory.INTEGER;
        if (target.isBooleanShape()) return TypeCategory.BOOLEAN;
        if (target.isDocumentShape()) return TypeCategory.DOCUMENT;
        if (isDocumentMap(target)) return TypeCategory.DOCUMENT_MAP;
        if (target.isEnumShape()) return TypeCategory.ENUM;
        if (target.isListShape()) {
            ListShape list = target.asListShape().get();
            Shape memberTarget = model.expectShape(list.getMember().getTarget());
            if (memberTarget.isStringShape() || memberTarget.isEnumShape()) {
                return TypeCategory.STRING_LIST;
            }
            if (memberTarget.isStructureShape()) return TypeCategory.STRUCT_LIST;
        }
        if (target.isMapShape()) return TypeCategory.DOCUMENT_MAP;
        return TypeCategory.OTHER;
    }

    private String toRustType(MemberShape member, boolean required) {
        Shape target = model.expectShape(member.getTarget());
        String baseType = toRustBaseType(target);
        if (required) {
            return baseType;
        }
        return "Option<" + baseType + ">";
    }

    private String toRustBaseType(Shape target) {
        if (target.isStringShape() || target.isEnumShape()) return "String";
        if (target.isIntegerShape()) return "i32";
        if (target.isLongShape()) return "i64";
        if (target.isBooleanShape()) return "bool";
        if (target.isDocumentShape()) return "serde_json::Value";
        if (isDocumentMap(target) || target.isMapShape()) return "serde_json::Value";
        if (target.isTimestampShape()) return "String";
        if (target.isListShape()) {
            ListShape list = target.asListShape().get();
            Shape memberTarget = model.expectShape(list.getMember().getTarget());
            return "Vec<" + toRustBaseType(memberTarget) + ">";
        }
        if (target.isStructureShape()) return "serde_json::Value";
        if (target.isUnionShape()) return "serde_json::Value";
        return "serde_json::Value";
    }

    private String toRustFieldName(String name) {
        String snake = toSnakeCase(name);
        // Handle Rust keywords
        if (snake.equals("override")) return "r#override";
        if (snake.equals("type")) return "r#type";
        if (snake.equals("match")) return "r#match";
        return snake;
    }

    static String toSnakeCase(String name) {
        if (name == null || name.isEmpty()) return name;
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            if (Character.isUpperCase(c)) {
                if (i > 0) sb.append('_');
                sb.append(Character.toLowerCase(c));
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    static String toPascalCase(String snake) {
        StringBuilder sb = new StringBuilder();
        boolean nextUpper = true;
        for (char c : snake.toCharArray()) {
            if (c == '_') {
                nextUpper = true;
            } else if (nextUpper) {
                sb.append(Character.toUpperCase(c));
                nextUpper = false;
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    // ========== Inner Types ==========

    static class MemberInfo {
        final MemberShape member;
        final boolean isRequired;

        MemberInfo(MemberShape member, boolean isRequired) {
            this.member = member;
            this.isRequired = isRequired;
        }
    }

    static class OperationInfo {
        final OperationShape operation;
        final String resourceName;
        final String verb;

        OperationInfo(OperationShape operation, String resourceName, String verb) {
            this.operation = operation;
            this.resourceName = resourceName;
            this.verb = verb;
        }

        String toolName() {
            return McpRustGenerator.toSnakeCase(resourceName) + "." + verb;
        }

        String paramStructName() {
            return toPascalCase(verb) + toPascalCase(McpRustGenerator.toSnakeCase(resourceName)) + "Params";
        }

        String implMethodName() {
            return verb + "_" + McpRustGenerator.toSnakeCase(resourceName) + "_impl";
        }

        String registrationMethodName() {
            return McpRustGenerator.toSnakeCase(resourceName) + "_" + verb;
        }

        String shortName() {
            return toPascalCase(verb) + resourceName;
        }
    }
}
