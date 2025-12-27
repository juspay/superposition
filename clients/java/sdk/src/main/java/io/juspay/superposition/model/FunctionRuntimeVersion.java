
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
public final class FunctionRuntimeVersion implements SerializableShape {
    public static final ShapeId $ID = ShapeId.from("io.superposition#FunctionRuntimeVersion");
    public static final FunctionRuntimeVersion V1 = new FunctionRuntimeVersion(Type.V1, "1.0");
    private static final List<FunctionRuntimeVersion> $TYPES = List.of(V1);

    public static final Schema $SCHEMA = Schema.createEnum($ID,
        Set.of(V1.value)
    );

    private final String value;
    private final Type type;

    private FunctionRuntimeVersion(Type type, String value) {
        this.type = Objects.requireNonNull(type, "type cannot be null");
        this.value = Objects.requireNonNull(value, "value cannot be null");
    }

    /**
     * Enum representing the possible variants of {@link FunctionRuntimeVersion}.
     */
    public enum Type {
        $UNKNOWN,
        V1
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
    public static FunctionRuntimeVersion unknown(String value) {
        return new FunctionRuntimeVersion(Type.$UNKNOWN, value);
    }

    /**
     * Returns an unmodifiable list containing the constants of this enum type, in the order declared.
     */
    public static List<FunctionRuntimeVersion> values() {
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
     * Returns a {@link FunctionRuntimeVersion} constant with the specified value.
     *
     * @param value value to create {@code FunctionRuntimeVersion} from.
     * @throws IllegalArgumentException if value does not match a known value.
     */
    public static FunctionRuntimeVersion from(String value) {
        return switch (value) {
            case "1.0" -> V1;
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
        FunctionRuntimeVersion that = (FunctionRuntimeVersion) other;
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
     * Builder for {@link FunctionRuntimeVersion}.
     */
    public static final class Builder implements ShapeBuilder<FunctionRuntimeVersion> {
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
        public FunctionRuntimeVersion build() {
            return switch (value) {
                case "1.0" -> V1;
                default -> new FunctionRuntimeVersion(Type.$UNKNOWN, value);
            };
        }

        @Override
        public Builder deserialize(ShapeDeserializer de) {
            return value(de.readString($SCHEMA));
        }
    }
}

