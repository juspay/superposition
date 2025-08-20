
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
 * Sort order enumeration for list operations.
 */
@SmithyGenerated
public final class SortBy implements SerializableShape {
    public static final ShapeId $ID = ShapeId.from("io.superposition#SortBy");
    /**
     * Descending order (Z-A, newest first)
     */
    public static final SortBy DESC = new SortBy(Type.DESC, "desc");
    /**
     * Ascending order (A-Z, oldest first)
     */
    public static final SortBy ASC = new SortBy(Type.ASC, "asc");
    private static final List<SortBy> $TYPES = List.of(DESC, ASC);

    public static final Schema $SCHEMA = Schema.createEnum($ID,
        Set.of(DESC.value, ASC.value)
    );

    private final String value;
    private final Type type;

    private SortBy(Type type, String value) {
        this.type = Objects.requireNonNull(type, "type cannot be null");
        this.value = Objects.requireNonNull(value, "value cannot be null");
    }

    /**
     * Enum representing the possible variants of {@link SortBy}.
     */
    public enum Type {
        $UNKNOWN,
        DESC,
        ASC
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
    public static SortBy unknown(String value) {
        return new SortBy(Type.$UNKNOWN, value);
    }

    /**
     * Returns an unmodifiable list containing the constants of this enum type, in the order declared.
     */
    public static List<SortBy> values() {
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
     * Returns a {@link SortBy} constant with the specified value.
     *
     * @param value value to create {@code SortBy} from.
     * @throws IllegalArgumentException if value does not match a known value.
     */
    public static SortBy from(String value) {
        return switch (value) {
            case "desc" -> DESC;
            case "asc" -> ASC;
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
        SortBy that = (SortBy) other;
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
     * Builder for {@link SortBy}.
     */
    public static final class Builder implements ShapeBuilder<SortBy> {
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
        public SortBy build() {
            return switch (value) {
                case "desc" -> DESC;
                case "asc" -> ASC;
                default -> new SortBy(Type.$UNKNOWN, value);
            };
        }

        @Override
        public Builder deserialize(ShapeDeserializer de) {
            return value(de.readString($SCHEMA));
        }
    }
}

