
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

@SmithyGenerated
public final class ExperimentSortOn implements SerializableShape {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ExperimentSortOn");
    public static final ExperimentSortOn LAST_MODIFIED_AT = new ExperimentSortOn(Type.LAST_MODIFIED_AT, "last_modified_at");
    public static final ExperimentSortOn CREATED_AT = new ExperimentSortOn(Type.CREATED_AT, "created_at");
    private static final List<ExperimentSortOn> $TYPES = List.of(LAST_MODIFIED_AT, CREATED_AT);

    public static final Schema $SCHEMA = Schema.createEnum($ID,
        Set.of(LAST_MODIFIED_AT.value, CREATED_AT.value)
    );

    private final String value;
    private final Type type;

    private ExperimentSortOn(Type type, String value) {
        this.type = Objects.requireNonNull(type, "type cannot be null");
        this.value = Objects.requireNonNull(value, "value cannot be null");
    }

    /**
     * Enum representing the possible variants of {@link ExperimentSortOn}.
     */
    public enum Type {
        $UNKNOWN,
        LAST_MODIFIED_AT,
        CREATED_AT
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
    public static ExperimentSortOn unknown(String value) {
        return new ExperimentSortOn(Type.$UNKNOWN, value);
    }

    /**
     * Returns an unmodifiable list containing the constants of this enum type, in the order declared.
     */
    public static List<ExperimentSortOn> values() {
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
     * Returns a {@link ExperimentSortOn} constant with the specified value.
     *
     * @param value value to create {@code ExperimentSortOn} from.
     * @throws IllegalArgumentException if value does not match a known value.
     */
    public static ExperimentSortOn from(String value) {
        return switch (value) {
            case "last_modified_at" -> LAST_MODIFIED_AT;
            case "created_at" -> CREATED_AT;
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
        ExperimentSortOn that = (ExperimentSortOn) other;
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
     * Builder for {@link ExperimentSortOn}.
     */
    public static final class Builder implements ShapeBuilder<ExperimentSortOn> {
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
        public ExperimentSortOn build() {
            return switch (value) {
                case "last_modified_at" -> LAST_MODIFIED_AT;
                case "created_at" -> CREATED_AT;
                default -> new ExperimentSortOn(Type.$UNKNOWN, value);
            };
        }

        @Override
        public Builder deserialize(ShapeDeserializer de) {
            return value(de.readString($SCHEMA));
        }
    }
}

