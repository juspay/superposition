
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
public final class ExperimentStatusType implements SerializableShape {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ExperimentStatusType");
    public static final ExperimentStatusType CREATED = new ExperimentStatusType(Type.CREATED, "CREATED");
    public static final ExperimentStatusType CONCLUDED = new ExperimentStatusType(Type.CONCLUDED, "CONCLUDED");
    public static final ExperimentStatusType INPROGRESS = new ExperimentStatusType(Type.INPROGRESS, "INPROGRESS");
    public static final ExperimentStatusType DISCARDED = new ExperimentStatusType(Type.DISCARDED, "DISCARDED");
    public static final ExperimentStatusType PAUSED = new ExperimentStatusType(Type.PAUSED, "PAUSED");
    private static final List<ExperimentStatusType> $TYPES = List.of(CREATED, CONCLUDED, INPROGRESS, DISCARDED, PAUSED);

    public static final Schema $SCHEMA = Schema.createEnum($ID,
        Set.of(CREATED.value, CONCLUDED.value, INPROGRESS.value, DISCARDED.value, PAUSED.value)
    );

    private final String value;
    private final Type type;

    private ExperimentStatusType(Type type, String value) {
        this.type = Objects.requireNonNull(type, "type cannot be null");
        this.value = Objects.requireNonNull(value, "value cannot be null");
    }

    /**
     * Enum representing the possible variants of {@link ExperimentStatusType}.
     */
    public enum Type {
        $UNKNOWN,
        CREATED,
        CONCLUDED,
        INPROGRESS,
        DISCARDED,
        PAUSED
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
    public static ExperimentStatusType unknown(String value) {
        return new ExperimentStatusType(Type.$UNKNOWN, value);
    }

    /**
     * Returns an unmodifiable list containing the constants of this enum type, in the order declared.
     */
    public static List<ExperimentStatusType> values() {
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
     * Returns a {@link ExperimentStatusType} constant with the specified value.
     *
     * @param value value to create {@code ExperimentStatusType} from.
     * @throws IllegalArgumentException if value does not match a known value.
     */
    public static ExperimentStatusType from(String value) {
        return switch (value) {
            case "CREATED" -> CREATED;
            case "CONCLUDED" -> CONCLUDED;
            case "INPROGRESS" -> INPROGRESS;
            case "DISCARDED" -> DISCARDED;
            case "PAUSED" -> PAUSED;
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
        ExperimentStatusType that = (ExperimentStatusType) other;
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
     * Builder for {@link ExperimentStatusType}.
     */
    public static final class Builder implements ShapeBuilder<ExperimentStatusType> {
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
        public ExperimentStatusType build() {
            return switch (value) {
                case "CREATED" -> CREATED;
                case "CONCLUDED" -> CONCLUDED;
                case "INPROGRESS" -> INPROGRESS;
                case "DISCARDED" -> DISCARDED;
                case "PAUSED" -> PAUSED;
                default -> new ExperimentStatusType(Type.$UNKNOWN, value);
            };
        }

        @Override
        public Builder deserialize(ShapeDeserializer de) {
            return value(de.readString($SCHEMA));
        }
    }
}

