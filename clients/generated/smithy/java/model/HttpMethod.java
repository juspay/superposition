
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
public final class HttpMethod implements SerializableShape {
    public static final ShapeId $ID = ShapeId.from("io.superposition#HttpMethod");
    public static final HttpMethod GET = new HttpMethod(Type.GET, "GET");
    public static final HttpMethod POST = new HttpMethod(Type.POST, "POST");
    public static final HttpMethod PUT = new HttpMethod(Type.PUT, "PUT");
    public static final HttpMethod PATCH = new HttpMethod(Type.PATCH, "PATCH");
    public static final HttpMethod DELETE = new HttpMethod(Type.DELETE, "DELETE");
    public static final HttpMethod HEAD = new HttpMethod(Type.HEAD, "HEAD");
    private static final List<HttpMethod> $TYPES = List.of(GET, POST, PUT, PATCH, DELETE, HEAD);

    public static final Schema $SCHEMA = Schema.createEnum($ID,
        Set.of(GET.value, POST.value, PUT.value, PATCH.value, DELETE.value, HEAD.value)
    );

    private final String value;
    private final Type type;

    private HttpMethod(Type type, String value) {
        this.type = Objects.requireNonNull(type, "type cannot be null");
        this.value = Objects.requireNonNull(value, "value cannot be null");
    }

    /**
     * Enum representing the possible variants of {@link HttpMethod}.
     */
    public enum Type {
        $UNKNOWN,
        GET,
        POST,
        PUT,
        PATCH,
        DELETE,
        HEAD
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
    public static HttpMethod unknown(String value) {
        return new HttpMethod(Type.$UNKNOWN, value);
    }

    /**
     * Returns an unmodifiable list containing the constants of this enum type, in the order declared.
     */
    public static List<HttpMethod> values() {
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
     * Returns a {@link HttpMethod} constant with the specified value.
     *
     * @param value value to create {@code HttpMethod} from.
     * @throws IllegalArgumentException if value does not match a known value.
     */
    public static HttpMethod from(String value) {
        return switch (value) {
            case "GET" -> GET;
            case "POST" -> POST;
            case "PUT" -> PUT;
            case "PATCH" -> PATCH;
            case "DELETE" -> DELETE;
            case "HEAD" -> HEAD;
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
        HttpMethod that = (HttpMethod) other;
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
     * Builder for {@link HttpMethod}.
     */
    public static final class Builder implements ShapeBuilder<HttpMethod> {
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
        public HttpMethod build() {
            return switch (value) {
                case "GET" -> GET;
                case "POST" -> POST;
                case "PUT" -> PUT;
                case "PATCH" -> PATCH;
                case "DELETE" -> DELETE;
                case "HEAD" -> HEAD;
                default -> new HttpMethod(Type.$UNKNOWN, value);
            };
        }

        @Override
        public Builder deserialize(ShapeDeserializer de) {
            return value(de.readString($SCHEMA));
        }
    }
}

