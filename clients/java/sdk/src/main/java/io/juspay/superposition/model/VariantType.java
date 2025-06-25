
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
public final class VariantType implements SerializableShape {
    public static final ShapeId $ID = ShapeId.from("io.superposition#VariantType");
    public static final VariantType CONTROL = new VariantType(Type.CONTROL, "CONTROL");
    public static final VariantType EXPERIMENTAL = new VariantType(Type.EXPERIMENTAL, "EXPERIMENTAL");
    private static final List<VariantType> $TYPES = List.of(CONTROL, EXPERIMENTAL);

    public static final Schema $SCHEMA = Schema.createEnum($ID,
        Set.of(CONTROL.value, EXPERIMENTAL.value)
    );

    private final String value;
    private final Type type;

    private VariantType(Type type, String value) {
        this.type = Objects.requireNonNull(type, "type cannot be null");
        this.value = Objects.requireNonNull(value, "value cannot be null");
    }

    /**
     * Enum representing the possible variants of {@link VariantType}.
     */
    public enum Type {
        $UNKNOWN,
        CONTROL,
        EXPERIMENTAL
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
    public static VariantType unknown(String value) {
        return new VariantType(Type.$UNKNOWN, value);
    }

    /**
     * Returns an unmodifiable list containing the constants of this enum type, in the order declared.
     */
    public static List<VariantType> values() {
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
     * Returns a {@link VariantType} constant with the specified value.
     *
     * @param value value to create {@code VariantType} from.
     * @throws IllegalArgumentException if value does not match a known value.
     */
    public static VariantType from(String value) {
        return switch (value) {
            case "CONTROL" -> CONTROL;
            case "EXPERIMENTAL" -> EXPERIMENTAL;
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
        VariantType that = (VariantType) other;
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
     * Builder for {@link VariantType}.
     */
    public static final class Builder implements ShapeBuilder<VariantType> {
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
        public VariantType build() {
            return switch (value) {
                case "CONTROL" -> CONTROL;
                case "EXPERIMENTAL" -> EXPERIMENTAL;
                default -> new VariantType(Type.$UNKNOWN, value);
            };
        }

        @Override
        public Builder deserialize(ShapeDeserializer de) {
            return value(de.readString($SCHEMA));
        }
    }
}

