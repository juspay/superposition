
package io.juspay.superposition.model;

import java.util.List;
import java.util.Objects;
import java.util.Set;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.SerializableShape;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

/**
 * How an import applies file entities to the workspace.
 */
@SmithyGenerated
public final class ImportStrategy implements SerializableShape {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ImportStrategy");
    /**
     * Create entities that are present in the file but missing from the workspace. Existing entities are
     * skipped. Nothing is deleted.
     */
    public static final ImportStrategy CREATE_ONLY = new ImportStrategy(Type.CREATE_ONLY, "create_only");
    /**
     * Create missing entities and update existing entities from the file. Entities absent from the file
     * are left untouched.
     */
    public static final ImportStrategy UPSERT = new ImportStrategy(Type.UPSERT, "upsert");
    /**
     * Mirror the file: create missing entities, update existing entities, and delete dimensions,
     * default-configs and contexts that are absent from it.
     */
    public static final ImportStrategy REPLACE = new ImportStrategy(Type.REPLACE, "replace");
    private static final List<ImportStrategy> $TYPES = List.of(CREATE_ONLY, UPSERT, REPLACE);

    public static final Schema $SCHEMA = Schema.createEnum($ID,
        Set.of(CREATE_ONLY.value, UPSERT.value, REPLACE.value)
    );

    private final String value;
    private final Type type;

    private ImportStrategy(Type type, String value) {
        this.type = Objects.requireNonNull(type, "type cannot be null");
        this.value = Objects.requireNonNull(value, "value cannot be null");
    }

    /**
     * Enum representing the possible variants of {@link ImportStrategy}.
     */
    public enum Type {
        $UNKNOWN,
        CREATE_ONLY,
        UPSERT,
        REPLACE
    }

    /**
     * Value contained by this Enum.
     */
    public String value() {
        return value;
    }

    /**
     * Type of this Enum variant.
     */
    public Type type() {
        return type;
    }

    /**
     * Create an Enum of an {@link Type#$UNKNOWN} type containing a value.
     *
     * @param value value contained by unknown Enum.
     */
    public static ImportStrategy unknown(String value) {
        return new ImportStrategy(Type.$UNKNOWN, value);
    }

    /**
     * Returns an unmodifiable list containing the constants of this enum type, in the order declared.
     */
    public static List<ImportStrategy> values() {
        return $TYPES;
    }

    @Override
    public void serialize(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA, this.value());
    }

    @Override
    public String toString() {
        return ToStringSerializer.serialize(this);
    }

    /**
     * Returns a {@link ImportStrategy} constant with the specified value.
     *
     * @param value value to create {@code ImportStrategy} from.
     * @throws IllegalArgumentException if value does not match a known value.
     */
    public static ImportStrategy from(String value) {
        return switch (value) {
            case "create_only" -> CREATE_ONLY;
            case "upsert" -> UPSERT;
            case "replace" -> REPLACE;
            default -> throw new IllegalArgumentException("Unknown value: " + value);
        };
    }

    @Override
    public boolean equals(Object other) {
        if (other == this) {
            return true;
        }
        if (other == null || getClass() != other.getClass()) {
            return false;
        }
        ImportStrategy that = (ImportStrategy) other;
        return this.value.equals(that.value);
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ImportStrategy}.
     */
    public static final class Builder implements ShapeBuilder<ImportStrategy> {
        private String value;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        private Builder value(String value) {
            this.value = Objects.requireNonNull(value, "Enum value cannot be null");
            return this;
        }

        @Override
        public ImportStrategy build() {
            return switch (value) {
                case "create_only" -> CREATE_ONLY;
                case "upsert" -> UPSERT;
                case "replace" -> REPLACE;
                default -> new ImportStrategy(Type.$UNKNOWN, value);
            };
        }

        @Override
        public Builder deserialize(ShapeDeserializer de) {
            return value(de.readString($SCHEMA));
        }
    }
}

