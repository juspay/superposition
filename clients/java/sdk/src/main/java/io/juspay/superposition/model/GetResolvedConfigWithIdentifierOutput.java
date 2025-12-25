
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
import software.amazon.smithy.java.core.serde.document.Document;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpPayloadTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class GetResolvedConfigWithIdentifierOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetResolvedConfigWithIdentifierOutput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("config", PreludeSchemas.DOCUMENT,
                new HttpPayloadTrait(),
                new RequiredTrait())
        .putMember("version", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-config-version"),
                new RequiredTrait())
        .putMember("last_modified", SharedSchemas.DATE_TIME,
                new HttpHeaderTrait("last-modified"),
                new RequiredTrait())
        .putMember("audit_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-audit-id"))
        .build();

    private static final Schema $SCHEMA_CONFIG = $SCHEMA.member("config");
    private static final Schema $SCHEMA_VERSION = $SCHEMA.member("version");
    private static final Schema $SCHEMA_LAST_MODIFIED = $SCHEMA.member("last_modified");
    private static final Schema $SCHEMA_AUDIT_ID = $SCHEMA.member("audit_id");

    private final transient Document config;
    private final transient String version;
    private final transient Instant lastModified;
    private final transient String auditId;

    private GetResolvedConfigWithIdentifierOutput(Builder builder) {
        this.config = builder.config;
        this.version = builder.version;
        this.lastModified = builder.lastModified;
        this.auditId = builder.auditId;
    }

    public Document config() {
        return config;
    }

    public String version() {
        return version;
    }

    public Instant lastModified() {
        return lastModified;
    }

    public String auditId() {
        return auditId;
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
        GetResolvedConfigWithIdentifierOutput that = (GetResolvedConfigWithIdentifierOutput) other;
        return Objects.equals(this.config, that.config)
               && Objects.equals(this.version, that.version)
               && Objects.equals(this.lastModified, that.lastModified)
               && Objects.equals(this.auditId, that.auditId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(config, version, lastModified, auditId);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeDocument($SCHEMA_CONFIG, config);
        serializer.writeString($SCHEMA_VERSION, version);
        serializer.writeTimestamp($SCHEMA_LAST_MODIFIED, lastModified);
        if (auditId != null) {
            serializer.writeString($SCHEMA_AUDIT_ID, auditId);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONFIG, member, config);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_VERSION, member, version);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, lastModified);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_AUDIT_ID, member, auditId);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link GetResolvedConfigWithIdentifierOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.config(this.config);
        builder.version(this.version);
        builder.lastModified(this.lastModified);
        builder.auditId(this.auditId);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link GetResolvedConfigWithIdentifierOutput}.
     */
    public static final class Builder implements ShapeBuilder<GetResolvedConfigWithIdentifierOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private Document config;
        private String version;
        private Instant lastModified;
        private String auditId;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder config(Document config) {
            this.config = Objects.requireNonNull(config, "config cannot be null");
            tracker.setMember($SCHEMA_CONFIG);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder version(String version) {
            this.version = Objects.requireNonNull(version, "version cannot be null");
            tracker.setMember($SCHEMA_VERSION);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder lastModified(Instant lastModified) {
            this.lastModified = Objects.requireNonNull(lastModified, "lastModified cannot be null");
            tracker.setMember($SCHEMA_LAST_MODIFIED);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder auditId(String auditId) {
            this.auditId = auditId;
            return this;
        }

        @Override
        public GetResolvedConfigWithIdentifierOutput build() {
            tracker.validate();
            return new GetResolvedConfigWithIdentifierOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> config((Document) SchemaUtils.validateSameMember($SCHEMA_CONFIG, member, value));
                case 1 -> version((String) SchemaUtils.validateSameMember($SCHEMA_VERSION, member, value));
                case 2 -> lastModified((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, value));
                case 3 -> auditId((String) SchemaUtils.validateSameMember($SCHEMA_AUDIT_ID, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<GetResolvedConfigWithIdentifierOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_CONFIG)) {
                tracker.setMember($SCHEMA_CONFIG);
            }
            if (!tracker.checkMember($SCHEMA_VERSION)) {
                version("");
            }
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED)) {
                lastModified(Instant.EPOCH);
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
                    case 0 -> builder.config(de.readDocument());
                    case 1 -> builder.version(de.readString(member));
                    case 2 -> builder.lastModified(de.readTimestamp(member));
                    case 3 -> builder.auditId(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

