
package io.juspay.superposition.model;

import java.time.Instant;
import java.util.Objects;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.PresenceTracker;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.SchemaUtils;
import software.amazon.smithy.java.core.schema.SerializableStruct;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpPayloadTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class GetConfigJsonOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetConfigJsonOutput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("json_config", PreludeSchemas.STRING,
                new RequiredTrait(),
                new HttpPayloadTrait())
        .putMember("last_modified", SharedSchemas.DATE_TIME,
                new HttpHeaderTrait("last-modified"))
        .build();

    private static final Schema $SCHEMA_JSON_CONFIG = $SCHEMA.member("json_config");
    private static final Schema $SCHEMA_LAST_MODIFIED = $SCHEMA.member("last_modified");

    private final transient String jsonConfig;
    private final transient Instant lastModified;

    private GetConfigJsonOutput(Builder builder) {
        this.jsonConfig = builder.jsonConfig;
        this.lastModified = builder.lastModified;
    }

    public String jsonConfig() {
        return jsonConfig;
    }

    public Instant lastModified() {
        return lastModified;
    }

    @Override
    public String toString() {
        return ToStringSerializer.serialize(this);
    }

    @Override
    public boolean equals(Object other) {
        if (other == this) {
            return true;
        }
        if (other == null || getClass() != other.getClass()) {
            return false;
        }
        GetConfigJsonOutput that = (GetConfigJsonOutput) other;
        return Objects.equals(this.jsonConfig, that.jsonConfig)
               && Objects.equals(this.lastModified, that.lastModified);
    }

    @Override
    public int hashCode() {
        return Objects.hash(jsonConfig, lastModified);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_JSON_CONFIG, jsonConfig);
        if (lastModified != null) {
            serializer.writeTimestamp($SCHEMA_LAST_MODIFIED, lastModified);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_JSON_CONFIG, member, jsonConfig);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, lastModified);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link GetConfigJsonOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.jsonConfig(this.jsonConfig);
        builder.lastModified(this.lastModified);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link GetConfigJsonOutput}.
     */
    public static final class Builder implements ShapeBuilder<GetConfigJsonOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String jsonConfig;
        private Instant lastModified;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder jsonConfig(String jsonConfig) {
            this.jsonConfig = Objects.requireNonNull(jsonConfig, "jsonConfig cannot be null");
            tracker.setMember($SCHEMA_JSON_CONFIG);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder lastModified(Instant lastModified) {
            this.lastModified = lastModified;
            return this;
        }

        @Override
        public GetConfigJsonOutput build() {
            tracker.validate();
            return new GetConfigJsonOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> jsonConfig((String) SchemaUtils.validateSameMember($SCHEMA_JSON_CONFIG, member, value));
                case 1 -> lastModified((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<GetConfigJsonOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_JSON_CONFIG)) {
                jsonConfig("");
            }
            return this;
        }

        @Override
        public Builder deserialize(ShapeDeserializer decoder) {
            decoder.readStruct($SCHEMA, this, $InnerDeserializer.INSTANCE);
            return this;
        }

        @Override
        public Builder deserializeMember(ShapeDeserializer decoder, Schema schema) {
            decoder.readStruct(schema.assertMemberTargetIs($SCHEMA), this, $InnerDeserializer.INSTANCE);
            return this;
        }

        private static final class $InnerDeserializer implements ShapeDeserializer.StructMemberConsumer<Builder> {
            private static final $InnerDeserializer INSTANCE = new $InnerDeserializer();

            @Override
            public void accept(Builder builder, Schema member, ShapeDeserializer de) {
                switch (member.memberIndex()) {
                    case 0 -> builder.jsonConfig(de.readString(member));
                    case 1 -> builder.lastModified(de.readTimestamp(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

