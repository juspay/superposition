
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
 * Type of background job.
 */
@SmithyGenerated
public final class BackgroundJobType implements SerializableShape {
    public static final ShapeId $ID = ShapeId.from("io.superposition#BackgroundJobType");
    public static final BackgroundJobType WEBHOOK = new BackgroundJobType(Type.WEBHOOK, "WEBHOOK");
    public static final BackgroundJobType PRIORITY_RECOMPUTE = new BackgroundJobType(Type.PRIORITY_RECOMPUTE, "PRIORITY_RECOMPUTE");
    public static final BackgroundJobType REDUCE = new BackgroundJobType(Type.REDUCE, "REDUCE");
    private static final List<BackgroundJobType> $TYPES = List.of(WEBHOOK, PRIORITY_RECOMPUTE, REDUCE);

    public static final Schema $SCHEMA = Schema.createEnum($ID,
        Set.of(WEBHOOK.value, PRIORITY_RECOMPUTE.value, REDUCE.value)
    );

    private final String value;
    private final Type type;

    private BackgroundJobType(Type type, String value) {
        this.type = Objects.requireNonNull(type, "type cannot be null");
        this.value = Objects.requireNonNull(value, "value cannot be null");
    }

    /**
     * Enum representing the possible variants of {@link BackgroundJobType}.
     */
    public enum Type {
        $UNKNOWN,
        WEBHOOK,
        PRIORITY_RECOMPUTE,
        REDUCE
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
    public static BackgroundJobType unknown(String value) {
        return new BackgroundJobType(Type.$UNKNOWN, value);
    }

    /**
     * Returns an unmodifiable list containing the constants of this enum type, in the order declared.
     */
    public static List<BackgroundJobType> values() {
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
     * Returns a {@link BackgroundJobType} constant with the specified value.
     *
     * @param value value to create {@code BackgroundJobType} from.
     * @throws IllegalArgumentException if value does not match a known value.
     */
    public static BackgroundJobType from(String value) {
        return switch (value) {
            case "WEBHOOK" -> WEBHOOK;
            case "PRIORITY_RECOMPUTE" -> PRIORITY_RECOMPUTE;
            case "REDUCE" -> REDUCE;
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
        BackgroundJobType that = (BackgroundJobType) other;
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
     * Builder for {@link BackgroundJobType}.
     */
    public static final class Builder implements ShapeBuilder<BackgroundJobType> {
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
        public BackgroundJobType build() {
            return switch (value) {
                case "WEBHOOK" -> WEBHOOK;
                case "PRIORITY_RECOMPUTE" -> PRIORITY_RECOMPUTE;
                case "REDUCE" -> REDUCE;
                default -> new BackgroundJobType(Type.$UNKNOWN, value);
            };
        }

        @Override
        public Builder deserialize(ShapeDeserializer de) {
            return value(de.readString($SCHEMA));
        }
    }
}

