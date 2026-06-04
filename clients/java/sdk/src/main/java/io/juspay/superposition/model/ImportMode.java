
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
 * How an import treats workspace entities that are not present in the imported file.
 */
@SmithyGenerated
public final class ImportMode implements SerializableShape {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ImportMode");
    /**
     * Upsert the entities in the file and leave everything else untouched.
     */
    public static final ImportMode MERGE = new ImportMode(Type.MERGE, "merge");
    /**
     * Mirror the file: additionally delete dimensions, default-configs and contexts that are absent from
     * it.
     */
    public static final ImportMode REPLACE = new ImportMode(Type.REPLACE, "replace");
    private static final List<ImportMode> $TYPES = List.of(MERGE, REPLACE);

    public static final Schema $SCHEMA = Schema.createEnum($ID,
        Set.of(MERGE.value, REPLACE.value)
    );

    private final String value;
    private final Type type;

    private ImportMode(Type type, String value) {
        this.type = Objects.requireNonNull(type, "type cannot be null");
        this.value = Objects.requireNonNull(value, "value cannot be null");
    }

    /**
     * Enum representing the possible variants of {@link ImportMode}.
     */
    public enum Type {
        $UNKNOWN,
        MERGE,
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
    public static ImportMode unknown(String value) {
        return new ImportMode(Type.$UNKNOWN, value);
    }

    /**
     * Returns an unmodifiable list containing the constants of this enum type, in the order declared.
     */
    public static List<ImportMode> values() {
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
     * Returns a {@link ImportMode} constant with the specified value.
     *
     * @param value value to create {@code ImportMode} from.
     * @throws IllegalArgumentException if value does not match a known value.
     */
    public static ImportMode from(String value) {
        return switch (value) {
            case "merge" -> MERGE;
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
        ImportMode that = (ImportMode) other;
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
     * Builder for {@link ImportMode}.
     */
    public static final class Builder implements ShapeBuilder<ImportMode> {
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
        public ImportMode build() {
            return switch (value) {
                case "merge" -> MERGE;
                case "replace" -> REPLACE;
                default -> new ImportMode(Type.$UNKNOWN, value);
            };
        }

        @Override
        public Builder deserialize(ShapeDeserializer de) {
            return value(de.readString($SCHEMA));
        }
    }
}

