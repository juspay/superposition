
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
public final class ExperimentGroupSortOn implements SerializableShape {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ExperimentGroupSortOn");
    /**
     * Sort by name.
     */
    public static final ExperimentGroupSortOn NAME = new ExperimentGroupSortOn(Type.NAME, "name");
    /**
     * Sort by creation timestamp.
     */
    public static final ExperimentGroupSortOn CREATED_AT = new ExperimentGroupSortOn(Type.CREATED_AT, "created_at");
    /**
     * Sort by last modification timestamp.
     */
    public static final ExperimentGroupSortOn LAST_MODIFIED_AT = new ExperimentGroupSortOn(Type.LAST_MODIFIED_AT, "last_modified_at");
    private static final List<ExperimentGroupSortOn> $TYPES = List.of(NAME, CREATED_AT, LAST_MODIFIED_AT);

    public static final Schema $SCHEMA = Schema.createEnum($ID,
        Set.of(NAME.value, CREATED_AT.value, LAST_MODIFIED_AT.value)
    );

    private final String value;
    private final Type type;

    private ExperimentGroupSortOn(Type type, String value) {
        this.type = Objects.requireNonNull(type, "type cannot be null");
        this.value = Objects.requireNonNull(value, "value cannot be null");
    }

    /**
     * Enum representing the possible variants of {@link ExperimentGroupSortOn}.
     */
    public enum Type {
        $UNKNOWN,
        NAME,
        CREATED_AT,
        LAST_MODIFIED_AT
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
    public static ExperimentGroupSortOn unknown(String value) {
        return new ExperimentGroupSortOn(Type.$UNKNOWN, value);
    }

    /**
     * Returns an unmodifiable list containing the constants of this enum type, in the order declared.
     */
    public static List<ExperimentGroupSortOn> values() {
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
     * Returns a {@link ExperimentGroupSortOn} constant with the specified value.
     *
     * @param value value to create {@code ExperimentGroupSortOn} from.
     * @throws IllegalArgumentException if value does not match a known value.
     */
    public static ExperimentGroupSortOn from(String value) {
        return switch (value) {
            case "name" -> NAME;
            case "created_at" -> CREATED_AT;
            case "last_modified_at" -> LAST_MODIFIED_AT;
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
        ExperimentGroupSortOn that = (ExperimentGroupSortOn) other;
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
     * Builder for {@link ExperimentGroupSortOn}.
     */
    public static final class Builder implements ShapeBuilder<ExperimentGroupSortOn> {
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
        public ExperimentGroupSortOn build() {
            return switch (value) {
                case "name" -> NAME;
                case "created_at" -> CREATED_AT;
                case "last_modified_at" -> LAST_MODIFIED_AT;
                default -> new ExperimentGroupSortOn(Type.$UNKNOWN, value);
            };
        }

        @Override
        public Builder deserialize(ShapeDeserializer de) {
            return value(de.readString($SCHEMA));
        }
    }
}

