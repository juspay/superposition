
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
 * How an import reacts when an individual entity fails to apply.
 */
@SmithyGenerated
public final class ImportOnError implements SerializableShape {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ImportOnError");
    /**
     * Roll the whole import back on the first error.
     */
    public static final ImportOnError ABORT = new ImportOnError(Type.ABORT, "abort");
    /**
     * Apply everything that is valid and report per-entity errors.
     */
    public static final ImportOnError CONTINUE = new ImportOnError(Type.CONTINUE, "continue");
    private static final List<ImportOnError> $TYPES = List.of(ABORT, CONTINUE);

    public static final Schema $SCHEMA = Schema.createEnum($ID,
        Set.of(ABORT.value, CONTINUE.value)
    );

    private final String value;
    private final Type type;

    private ImportOnError(Type type, String value) {
        this.type = Objects.requireNonNull(type, "type cannot be null");
        this.value = Objects.requireNonNull(value, "value cannot be null");
    }

    /**
     * Enum representing the possible variants of {@link ImportOnError}.
     */
    public enum Type {
        $UNKNOWN,
        ABORT,
        CONTINUE
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
    public static ImportOnError unknown(String value) {
        return new ImportOnError(Type.$UNKNOWN, value);
    }

    /**
     * Returns an unmodifiable list containing the constants of this enum type, in the order declared.
     */
    public static List<ImportOnError> values() {
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
     * Returns a {@link ImportOnError} constant with the specified value.
     *
     * @param value value to create {@code ImportOnError} from.
     * @throws IllegalArgumentException if value does not match a known value.
     */
    public static ImportOnError from(String value) {
        return switch (value) {
            case "abort" -> ABORT;
            case "continue" -> CONTINUE;
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
        ImportOnError that = (ImportOnError) other;
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
     * Builder for {@link ImportOnError}.
     */
    public static final class Builder implements ShapeBuilder<ImportOnError> {
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
        public ImportOnError build() {
            return switch (value) {
                case "abort" -> ABORT;
                case "continue" -> CONTINUE;
                default -> new ImportOnError(Type.$UNKNOWN, value);
            };
        }

        @Override
        public Builder deserialize(ShapeDeserializer de) {
            return value(de.readString($SCHEMA));
        }
    }
}

