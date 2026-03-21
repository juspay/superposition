package io.superposition.cli.codegen;

import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.knowledge.HttpBinding;
import software.amazon.smithy.model.knowledge.HttpBindingIndex;
import software.amazon.smithy.model.shapes.*;
import software.amazon.smithy.model.traits.*;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Walks the Smithy model to extract resource and operation information
 * for CLI code generation.
 */
public final class ModelWalker {

    private final Model model;
    private final ServiceShape service;
    private final ArgMapper argMapper;
    private final HttpBindingIndex httpBindingIndex;

    /** Mixin field names that become global CLI args (not per-command). */
    private static final Set<String> GLOBAL_FIELDS = Set.of(
            "workspace_id", "org_id"
    );

    public ModelWalker(Model model, ServiceShape service) {
        this.model = model;
        this.service = service;
        this.argMapper = new ArgMapper(model);
        this.httpBindingIndex = HttpBindingIndex.of(model);
    }

    /**
     * Extracts all resources and their operations from the service.
     */
    public List<ResourceInfo> walk() {
        List<ResourceInfo> resources = new ArrayList<>();

        for (ShapeId resourceId : service.getResources()) {
            ResourceShape resource = model.expectShape(resourceId, ResourceShape.class);
            ResourceInfo info = extractResource(resource);
            if (!info.getOperations().isEmpty()) {
                resources.add(info);
            }
        }

        // Sort resources alphabetically for stable output
        resources.sort(Comparator.comparing(ResourceInfo::getCliSubcommandName));
        return resources;
    }

    private ResourceInfo extractResource(ResourceShape resource) {
        String resourceName = resource.getId().getName();
        String cliName = ArgMapper.toKebabCase(resourceName);
        String moduleName = toSnakeCase(resourceName);

        List<OperationInfo> operations = new ArrayList<>();

        // Collect all operation IDs from the resource
        List<ShapeId> opIds = new ArrayList<>();

        resource.getCreate().ifPresent(opIds::add);
        resource.getRead().ifPresent(opIds::add);
        resource.getUpdate().ifPresent(opIds::add);
        resource.getDelete().ifPresent(opIds::add);
        resource.getList().ifPresent(opIds::add);
        resource.getPut().ifPresent(opIds::add);
        opIds.addAll(resource.getOperations());
        opIds.addAll(resource.getCollectionOperations());

        for (ShapeId opId : opIds) {
            OperationShape operation = model.expectShape(opId, OperationShape.class);
            OperationInfo opInfo = extractOperation(operation);
            if (opInfo != null) {
                operations.add(opInfo);
            }
        }

        return new ResourceInfo(resourceName, cliName, moduleName, operations);
    }

    private OperationInfo extractOperation(OperationShape operation) {
        String opName = operation.getId().getName();

        // Extract HTTP trait
        Optional<HttpTrait> httpTraitOpt = operation.getTrait(HttpTrait.class);
        String httpMethod = httpTraitOpt.map(t -> t.getMethod()).orElse("POST");
        String httpUri = httpTraitOpt.map(t -> t.getUri().toString()).orElse("/");

        // Extract documentation
        String docs = operation.getTrait(DocumentationTrait.class)
                .map(DocumentationTrait::getValue)
                .orElse("");

        // Derive CLI command name from operation name
        String cliCommandName = deriveCommandName(opName);
        String sdkMethodName = toSnakeCase(opName);

        // Extract input shape and its members
        Optional<ShapeId> inputId = operation.getInputShape()
                .equals(ShapeId.from("smithy.api#Unit"))
                ? Optional.empty()
                : Optional.of(operation.getInputShape());

        List<ArgMapper.ClapArg> args = new ArrayList<>();
        boolean hasPagination = false;
        boolean hasWorkspaceMixin = false;
        boolean hasOrganisationMixin = false;
        boolean hasBodyPayload = false;

        if (inputId.isPresent()) {
            StructureShape inputShape = model.expectShape(inputId.get(), StructureShape.class);

            // Check mixins
            for (ShapeId mixinId : inputShape.getMixins()) {
                String mixinName = mixinId.getName();
                if (mixinName.contains("PaginationParams")) {
                    hasPagination = true;
                }
                if (mixinName.contains("WorkspaceMixin")) {
                    hasWorkspaceMixin = true;
                }
                if (mixinName.contains("OrganisationMixin")) {
                    hasOrganisationMixin = true;
                }
            }

            // Also check member names to detect mixins applied transitively
            Set<String> memberNames = inputShape.getAllMembers().keySet();
            if (memberNames.contains("workspace_id")) {
                hasWorkspaceMixin = true;
            }
            if (memberNames.contains("org_id") && !hasWorkspaceMixin) {
                hasOrganisationMixin = true;
            }
            if (memberNames.contains("count") && memberNames.contains("page")) {
                hasPagination = true;
            }

            // Process each member
            for (Map.Entry<String, MemberShape> entry : inputShape.getAllMembers().entrySet()) {
                String fieldName = entry.getKey();
                MemberShape member = entry.getValue();

                // Skip global fields (handled at top level)
                if (GLOBAL_FIELDS.contains(fieldName)) {
                    continue;
                }

                Shape targetShape = model.expectShape(member.getTarget());
                ArgMapper.ClapArg arg = argMapper.toClapArg(member, targetShape);

                if (arg.binding == ArgMapper.ClapArg.Binding.BODY) {
                    hasBodyPayload = true;
                }

                args.add(arg);
            }
        }

        // Sort: positional args first, then required named, then optional
        args.sort((a, b) -> {
            if (a.binding == ArgMapper.ClapArg.Binding.POSITIONAL
                    && b.binding != ArgMapper.ClapArg.Binding.POSITIONAL) return -1;
            if (b.binding == ArgMapper.ClapArg.Binding.POSITIONAL
                    && a.binding != ArgMapper.ClapArg.Binding.POSITIONAL) return 1;
            if (a.required && !b.required) return -1;
            if (b.required && !a.required) return 1;
            return a.cliName.compareTo(b.cliName);
        });

        return new OperationInfo(
                opName, cliCommandName, httpMethod, httpUri,
                docs, args, hasPagination, hasWorkspaceMixin,
                hasOrganisationMixin, hasBodyPayload, sdkMethodName);
    }

    /**
     * Derives a CLI-friendly command name from a Smithy operation name.
     * E.g., "CreateDimension" -> "create", "ListDefaultConfigs" -> "list",
     * "GetConfigFast" -> "get-fast", "RampExperiment" -> "ramp"
     */
    private String deriveCommandName(String operationName) {
        // Common CRUD prefixes that map to simple command names
        // when the suffix matches the resource name
        String[] crudPrefixes = {
                "Create", "Get", "List", "Update", "Delete", "Put"
        };

        for (String prefix : crudPrefixes) {
            if (operationName.startsWith(prefix)) {
                String remainder = operationName.substring(prefix.length());
                // If the remainder is just the resource name (or plural), use the prefix
                // Otherwise, combine prefix + distinguishing suffix
                if (isResourceName(remainder)) {
                    return prefix.toLowerCase();
                } else {
                    return ArgMapper.toKebabCase(prefix + remainder)
                            .replaceAll("-+$", "");
                }
            }
        }

        // For non-CRUD operations, use the full name in kebab-case
        return ArgMapper.toKebabCase(operationName);
    }

    private boolean isResourceName(String name) {
        // Check if this name (or its plural/singular) matches a known resource
        for (ShapeId resId : service.getResources()) {
            String resName = resId.getName();
            if (name.equals(resName)
                    || name.equals(resName + "s")
                    || (name + "s").equals(resName)
                    || name.equals(resName + "es")
                    || (name + "es").equals(resName)) {
                return true;
            }
        }
        return false;
    }

    private static String toSnakeCase(String input) {
        return input
                .replaceAll("([a-z])([A-Z])", "$1_$2")
                .replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
                .toLowerCase();
    }
}
