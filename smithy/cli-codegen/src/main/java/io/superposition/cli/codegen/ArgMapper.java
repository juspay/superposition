package io.superposition.cli.codegen;

import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.shapes.*;
import software.amazon.smithy.model.traits.*;

import java.util.*;

/**
 * Maps Smithy shapes to Rust/clap argument types and generates
 * the appropriate clap argument definitions.
 */
public final class ArgMapper {

    private final Model model;

    public ArgMapper(Model model) {
        this.model = model;
    }

    /**
     * Returns the Rust type string for a given Smithy shape.
     */
    public String rustType(Shape shape) {
        return switch (shape.getType()) {
            case STRING -> {
                if (shape.hasTrait(EnumTrait.class) || shape instanceof EnumShape) {
                    yield "String"; // We'll validate via possible_values
                }
                yield "String";
            }
            case INTEGER, INT_ENUM -> "i64";
            case LONG -> "i64";
            case FLOAT -> "f64";
            case DOUBLE -> "f64";
            case BOOLEAN -> "bool";
            case TIMESTAMP -> "String"; // ISO-8601 string
            case LIST -> "Vec<String>";
            case MAP -> "Vec<String>"; // KEY=VALUE pairs
            case DOCUMENT -> "String"; // Raw JSON
            case BLOB -> "String"; // Base64
            case UNION -> "String"; // Serialized as string for CLI
            default -> "String";
        };
    }

    /**
     * Returns the clap arg configuration for a given member shape.
     */
    public ClapArg toClapArg(MemberShape member, Shape targetShape) {
        String fieldName = member.getMemberName();
        String cliName = toKebabCase(fieldName);
        boolean required = member.hasTrait(RequiredTrait.class);
        String help = member.getTrait(DocumentationTrait.class)
                .map(DocumentationTrait::getValue)
                .orElse("");
        String rustType = rustType(targetShape);

        // Determine binding type
        ClapArg.Binding binding;
        if (member.hasTrait(HttpLabelTrait.class)) {
            binding = ClapArg.Binding.POSITIONAL;
        } else if (member.hasTrait(HttpQueryTrait.class)) {
            binding = ClapArg.Binding.NAMED;
        } else if (member.hasTrait(HttpHeaderTrait.class)) {
            binding = ClapArg.Binding.NAMED;
        } else if (member.hasTrait(HttpPayloadTrait.class)) {
            binding = ClapArg.Binding.BODY;
        } else {
            binding = ClapArg.Binding.NAMED;
        }

        // Check for enum values
        List<String> enumValues = getEnumValues(targetShape);

        // Check if this is a list type (allows multiple values)
        boolean multiple = targetShape.getType() == ShapeType.LIST;

        // Check if this is a boolean flag
        boolean isFlag = targetShape.getType() == ShapeType.BOOLEAN;

        // Check if this is a map type (KEY=VALUE)
        boolean isKeyValue = targetShape.getType() == ShapeType.MAP;

        return new ClapArg(
                fieldName,
                cliName,
                rustType,
                required,
                help,
                binding,
                enumValues,
                multiple,
                isFlag,
                isKeyValue
        );
    }

    /**
     * Extracts enum values from an enum shape.
     */
    public List<String> getEnumValues(Shape shape) {
        List<String> values = new ArrayList<>();

        if (shape instanceof EnumShape enumShape) {
            for (Map.Entry<String, MemberShape> entry : enumShape.getAllMembers().entrySet()) {
                values.add(entry.getKey());
            }
        } else if (shape.hasTrait(EnumTrait.class)) {
            EnumTrait enumTrait = shape.expectTrait(EnumTrait.class);
            for (EnumDefinition def : enumTrait.getValues()) {
                def.getName().ifPresentOrElse(
                        values::add,
                        () -> values.add(def.getValue())
                );
            }
        }
        return values;
    }

    /**
     * Converts a snake_case or camelCase string to kebab-case.
     */
    public static String toKebabCase(String input) {
        return input
                .replaceAll("([a-z])([A-Z])", "$1-$2")
                .replaceAll("_", "-")
                .toLowerCase();
    }

    /**
     * Converts a snake_case string to PascalCase.
     */
    public static String toPascalCase(String input) {
        StringBuilder result = new StringBuilder();
        boolean capitalizeNext = true;
        for (char c : input.toCharArray()) {
            if (c == '_' || c == '-') {
                capitalizeNext = true;
            } else if (capitalizeNext) {
                result.append(Character.toUpperCase(c));
                capitalizeNext = false;
            } else {
                result.append(c);
            }
        }
        return result.toString();
    }

    /**
     * Represents a mapped clap argument.
     */
    public static final class ClapArg {
        public enum Binding {
            POSITIONAL,
            NAMED,
            BODY
        }

        public final String fieldName;
        public final String cliName;
        public final String rustType;
        public final boolean required;
        public final String help;
        public final Binding binding;
        public final List<String> enumValues;
        public final boolean multiple;
        public final boolean isFlag;
        public final boolean isKeyValue;

        public ClapArg(
                String fieldName,
                String cliName,
                String rustType,
                boolean required,
                String help,
                Binding binding,
                List<String> enumValues,
                boolean multiple,
                boolean isFlag,
                boolean isKeyValue) {
            this.fieldName = fieldName;
            this.cliName = cliName;
            this.rustType = rustType;
            this.required = required;
            this.help = help;
            this.binding = binding;
            this.enumValues = enumValues;
            this.multiple = multiple;
            this.isFlag = isFlag;
            this.isKeyValue = isKeyValue;
        }
    }
}
