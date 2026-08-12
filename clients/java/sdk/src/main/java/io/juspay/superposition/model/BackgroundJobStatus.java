
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
 * Lifecycle status of a background job.
 */
@SmithyGenerated
public final class BackgroundJobStatus implements SerializableShape {
    public static final ShapeId $ID = ShapeId.from("io.superposition#BackgroundJobStatus");
    public static final BackgroundJobStatus CREATED = new BackgroundJobStatus(Type.CREATED, "CREATED");
    public static final BackgroundJobStatus SCHEDULED = new BackgroundJobStatus(Type.SCHEDULED, "SCHEDULED");
    public static final BackgroundJobStatus INPROGRESS = new BackgroundJobStatus(Type.INPROGRESS, "INPROGRESS");
    public static final BackgroundJobStatus FAILED = new BackgroundJobStatus(Type.FAILED, "FAILED");
    public static final BackgroundJobStatus COMPLETED = new BackgroundJobStatus(Type.COMPLETED, "COMPLETED");
    private static final List<BackgroundJobStatus> $TYPES = List.of(CREATED, SCHEDULED, INPROGRESS, FAILED, COMPLETED);

    public static final Schema $SCHEMA = Schema.createEnum($ID,
        Set.of(CREATED.value, SCHEDULED.value, INPROGRESS.value, FAILED.value, COMPLETED.value)
    );

    private final String value;
    private final Type type;

    private BackgroundJobStatus(Type type, String value) {
        this.type = Objects.requireNonNull(type, "type cannot be null");
        this.value = Objects.requireNonNull(value, "value cannot be null");
    }

    /**
     * Enum representing the possible variants of {@link BackgroundJobStatus}.
     */
    public enum Type {
        $UNKNOWN,
        CREATED,
        SCHEDULED,
        INPROGRESS,
        FAILED,
        COMPLETED
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
    public static BackgroundJobStatus unknown(String value) {
        return new BackgroundJobStatus(Type.$UNKNOWN, value);
    }

    /**
     * Returns an unmodifiable list containing the constants of this enum type, in the order declared.
     */
    public static List<BackgroundJobStatus> values() {
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
     * Returns a {@link BackgroundJobStatus} constant with the specified value.
     *
     * @param value value to create {@code BackgroundJobStatus} from.
     * @throws IllegalArgumentException if value does not match a known value.
     */
    public static BackgroundJobStatus from(String value) {
        return switch (value) {
            case "CREATED" -> CREATED;
            case "SCHEDULED" -> SCHEDULED;
            case "INPROGRESS" -> INPROGRESS;
            case "FAILED" -> FAILED;
            case "COMPLETED" -> COMPLETED;
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
        BackgroundJobStatus that = (BackgroundJobStatus) other;
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
     * Builder for {@link BackgroundJobStatus}.
     */
    public static final class Builder implements ShapeBuilder<BackgroundJobStatus> {
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
        public BackgroundJobStatus build() {
            return switch (value) {
                case "CREATED" -> CREATED;
                case "SCHEDULED" -> SCHEDULED;
                case "INPROGRESS" -> INPROGRESS;
                case "FAILED" -> FAILED;
                case "COMPLETED" -> COMPLETED;
                default -> new BackgroundJobStatus(Type.$UNKNOWN, value);
            };
        }

        @Override
        public Builder deserialize(ShapeDeserializer de) {
            return value(de.readString($SCHEMA));
        }
    }
}

