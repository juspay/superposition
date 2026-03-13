
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
public final class GetConfigTomlOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetConfigTomlOutput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("toml_config", PreludeSchemas.STRING,
                new RequiredTrait(),
                new HttpPayloadTrait())
        .putMember("last_modified", SharedSchemas.DATE_TIME,
                new HttpHeaderTrait("last-modified"))
        .build();

    private static final Schema $SCHEMA_TOML_CONFIG = $SCHEMA.member("toml_config");
    private static final Schema $SCHEMA_LAST_MODIFIED = $SCHEMA.member("last_modified");

    private final transient String tomlConfig;
    private final transient Instant lastModified;

    private GetConfigTomlOutput(Builder builder) {
        this.tomlConfig = builder.tomlConfig;
        this.lastModified = builder.lastModified;
    }

    public String tomlConfig() {
        return tomlConfig;
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
        GetConfigTomlOutput that = (GetConfigTomlOutput) other;
        return Objects.equals(this.tomlConfig, that.tomlConfig)
               && Objects.equals(this.lastModified, that.lastModified);
    }

    @Override
    public int hashCode() {
        return Objects.hash(tomlConfig, lastModified);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_TOML_CONFIG, tomlConfig);
        if (lastModified != null) {
            serializer.writeTimestamp($SCHEMA_LAST_MODIFIED, lastModified);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_TOML_CONFIG, member, tomlConfig);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, lastModified);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link GetConfigTomlOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.tomlConfig(this.tomlConfig);
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
     * Builder for {@link GetConfigTomlOutput}.
     */
    public static final class Builder implements ShapeBuilder<GetConfigTomlOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String tomlConfig;
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
        public Builder tomlConfig(String tomlConfig) {
            this.tomlConfig = Objects.requireNonNull(tomlConfig, "tomlConfig cannot be null");
            tracker.setMember($SCHEMA_TOML_CONFIG);
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
        public GetConfigTomlOutput build() {
            tracker.validate();
            return new GetConfigTomlOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> tomlConfig((String) SchemaUtils.validateSameMember($SCHEMA_TOML_CONFIG, member, value));
                case 1 -> lastModified((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<GetConfigTomlOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_TOML_CONFIG)) {
                tomlConfig("");
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
                    case 0 -> builder.tomlConfig(de.readString(member));
                    case 1 -> builder.lastModified(de.readTimestamp(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

