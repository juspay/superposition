
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
 * Strategy to follow while filter items based on the context
 */
@SmithyGenerated
public final class DimensionMatchStrategy implements SerializableShape {
    public static final ShapeId $ID = ShapeId.from("io.superposition#DimensionMatchStrategy");
    /**
     * Match the overrides which have the exact context
     */
    public static final DimensionMatchStrategy EXACT = new DimensionMatchStrategy(Type.EXACT, "exact");
    /**
     * Match the overrides which have the given context as subset
     */
    public static final DimensionMatchStrategy SUBSET = new DimensionMatchStrategy(Type.SUBSET, "subset");
    /**
     * Match overrides whose context does not conflict with any supplied query dimension. This is useful
     * for fetching candidates for later local evaluation.
     */
    public static final DimensionMatchStrategy NON_CONFLICTING = new DimensionMatchStrategy(Type.NON_CONFLICTING, "non_conflicting");
    private static final List<DimensionMatchStrategy> $TYPES = List.of(EXACT, SUBSET, NON_CONFLICTING);

    public static final Schema $SCHEMA = Schema.createEnum($ID,
        Set.of(EXACT.value, SUBSET.value, NON_CONFLICTING.value)
    );

    private final String value;
    private final Type type;

    private DimensionMatchStrategy(Type type, String value) {
        this.type = Objects.requireNonNull(type, "type cannot be null");
        this.value = Objects.requireNonNull(value, "value cannot be null");
    }

    /**
     * Enum representing the possible variants of {@link DimensionMatchStrategy}.
     */
    public enum Type {
        $UNKNOWN,
        EXACT,
        SUBSET,
        NON_CONFLICTING
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
    public static DimensionMatchStrategy unknown(String value) {
        return new DimensionMatchStrategy(Type.$UNKNOWN, value);
    }

    /**
     * Returns an unmodifiable list containing the constants of this enum type, in the order declared.
     */
    public static List<DimensionMatchStrategy> values() {
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
     * Returns a {@link DimensionMatchStrategy} constant with the specified value.
     *
     * @param value value to create {@code DimensionMatchStrategy} from.
     * @throws IllegalArgumentException if value does not match a known value.
     */
    public static DimensionMatchStrategy from(String value) {
        return switch (value) {
            case "exact" -> EXACT;
            case "subset" -> SUBSET;
            case "non_conflicting" -> NON_CONFLICTING;
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
        DimensionMatchStrategy that = (DimensionMatchStrategy) other;
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
     * Builder for {@link DimensionMatchStrategy}.
     */
    public static final class Builder implements ShapeBuilder<DimensionMatchStrategy> {
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
        public DimensionMatchStrategy build() {
            return switch (value) {
                case "exact" -> EXACT;
                case "subset" -> SUBSET;
                case "non_conflicting" -> NON_CONFLICTING;
                default -> new DimensionMatchStrategy(Type.$UNKNOWN, value);
            };
        }

        @Override
        public Builder deserialize(ShapeDeserializer de) {
            return value(de.readString($SCHEMA));
        }
    }
}

