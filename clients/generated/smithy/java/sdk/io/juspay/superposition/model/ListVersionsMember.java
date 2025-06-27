
package io.juspay.superposition.model;

import java.time.Instant;
import java.util.Collections;
import java.util.List;
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
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class ListVersionsMember implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListVersionsMember");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("config", PreludeSchemas.DOCUMENT,
                new RequiredTrait())
        .putMember("config_hash", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("created_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("description", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("tags", SharedSchemas.STRING_LIST)
        .build();

    private static final Schema $SCHEMA_ID = $SCHEMA.member("id");
    private static final Schema $SCHEMA_CONFIG = $SCHEMA.member("config");
    private static final Schema $SCHEMA_CONFIG_HASH = $SCHEMA.member("config_hash");
    private static final Schema $SCHEMA_CREATED_AT = $SCHEMA.member("created_at");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_TAGS = $SCHEMA.member("tags");

    private final transient String id;
    private final transient Document config;
    private final transient String configHash;
    private final transient Instant createdAt;
    private final transient String description;
    private final transient List<String> tags;

    private ListVersionsMember(Builder builder) {
        this.id = builder.id;
        this.config = builder.config;
        this.configHash = builder.configHash;
        this.createdAt = builder.createdAt;
        this.description = builder.description;
        this.tags = builder.tags == null ? null : Collections.unmodifiableList(builder.tags);
    }

    public String id() {
        return id;
    }

    public Document config() {
        return config;
    }

    public String configHash() {
        return configHash;
    }

    public Instant createdAt() {
        return createdAt;
    }

    public String description() {
        return description;
    }

    public List<String> tags() {
        if (tags == null) {
            return Collections.emptyList();
        }
        return tags;
    }

    public boolean hasTags() {
        return tags != null;
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
        ListVersionsMember that = (ListVersionsMember) other;
        return Objects.equals(this.id, that.id)
               && Objects.equals(this.config, that.config)
               && Objects.equals(this.configHash, that.configHash)
               && Objects.equals(this.createdAt, that.createdAt)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.tags, that.tags);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, config, configHash, createdAt, description, tags);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_ID, id);
        serializer.writeDocument($SCHEMA_CONFIG, config);
        serializer.writeString($SCHEMA_CONFIG_HASH, configHash);
        serializer.writeTimestamp($SCHEMA_CREATED_AT, createdAt);
        serializer.writeString($SCHEMA_DESCRIPTION, description);
        if (tags != null) {
            serializer.writeList($SCHEMA_TAGS, tags, tags.size(), SharedSerde.StringListSerializer.INSTANCE);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_ID, member, id);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONFIG, member, config);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONFIG_HASH, member, configHash);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, createdAt);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_TAGS, member, tags);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ListVersionsMember}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.id(this.id);
        builder.config(this.config);
        builder.configHash(this.configHash);
        builder.createdAt(this.createdAt);
        builder.description(this.description);
        builder.tags(this.tags);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ListVersionsMember}.
     */
    public static final class Builder implements ShapeBuilder<ListVersionsMember> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String id;
        private Document config;
        private String configHash;
        private Instant createdAt;
        private String description;
        private List<String> tags;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder id(String id) {
            this.id = Objects.requireNonNull(id, "id cannot be null");
            tracker.setMember($SCHEMA_ID);
            return this;
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
        public Builder configHash(String configHash) {
            this.configHash = Objects.requireNonNull(configHash, "configHash cannot be null");
            tracker.setMember($SCHEMA_CONFIG_HASH);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder createdAt(Instant createdAt) {
            this.createdAt = Objects.requireNonNull(createdAt, "createdAt cannot be null");
            tracker.setMember($SCHEMA_CREATED_AT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder description(String description) {
            this.description = Objects.requireNonNull(description, "description cannot be null");
            tracker.setMember($SCHEMA_DESCRIPTION);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder tags(List<String> tags) {
            this.tags = tags;
            return this;
        }

        @Override
        public ListVersionsMember build() {
            tracker.validate();
            return new ListVersionsMember(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> id((String) SchemaUtils.validateSameMember($SCHEMA_ID, member, value));
                case 1 -> config((Document) SchemaUtils.validateSameMember($SCHEMA_CONFIG, member, value));
                case 2 -> configHash((String) SchemaUtils.validateSameMember($SCHEMA_CONFIG_HASH, member, value));
                case 3 -> createdAt((Instant) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, value));
                case 4 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 5 -> tags((List<String>) SchemaUtils.validateSameMember($SCHEMA_TAGS, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ListVersionsMember> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_ID)) {
                id("");
            }
            if (!tracker.checkMember($SCHEMA_CONFIG)) {
                tracker.setMember($SCHEMA_CONFIG);
            }
            if (!tracker.checkMember($SCHEMA_CONFIG_HASH)) {
                configHash("");
            }
            if (!tracker.checkMember($SCHEMA_CREATED_AT)) {
                createdAt(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_DESCRIPTION)) {
                description("");
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
                    case 0 -> builder.id(de.readString(member));
                    case 1 -> builder.config(de.readDocument());
                    case 2 -> builder.configHash(de.readString(member));
                    case 3 -> builder.createdAt(de.readTimestamp(member));
                    case 4 -> builder.description(de.readString(member));
                    case 5 -> builder.tags(SharedSerde.deserializeStringList(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

