
package io.juspay.superposition.model;

import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.Map;
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
public final class UpdateWebhookOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#WebhookResponse");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("name", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("description", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("enabled", PreludeSchemas.BOOLEAN,
                new RequiredTrait())
        .putMember("url", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("method", HttpMethod.$SCHEMA,
                new RequiredTrait())
        .putMember("version", Version.$SCHEMA,
                new RequiredTrait())
        .putMember("custom_headers", SharedSchemas.OBJECT)
        .putMember("events", SharedSchemas.EVENTS,
                new RequiredTrait())
        .putMember("max_retries", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("last_triggered_at", SharedSchemas.DATE_TIME)
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("created_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("created_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("last_modified_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("last_modified_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_NAME = $SCHEMA.member("name");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_ENABLED = $SCHEMA.member("enabled");
    private static final Schema $SCHEMA_URL = $SCHEMA.member("url");
    private static final Schema $SCHEMA_METHOD = $SCHEMA.member("method");
    private static final Schema $SCHEMA_VERSION = $SCHEMA.member("version");
    private static final Schema $SCHEMA_CUSTOM_HEADERS = $SCHEMA.member("custom_headers");
    private static final Schema $SCHEMA_EVENTS = $SCHEMA.member("events");
    private static final Schema $SCHEMA_MAX_RETRIES = $SCHEMA.member("max_retries");
    private static final Schema $SCHEMA_LAST_TRIGGERED_AT = $SCHEMA.member("last_triggered_at");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");
    private static final Schema $SCHEMA_CREATED_BY = $SCHEMA.member("created_by");
    private static final Schema $SCHEMA_CREATED_AT = $SCHEMA.member("created_at");
    private static final Schema $SCHEMA_LAST_MODIFIED_BY = $SCHEMA.member("last_modified_by");
    private static final Schema $SCHEMA_LAST_MODIFIED_AT = $SCHEMA.member("last_modified_at");

    private final transient String name;
    private final transient String description;
    private final transient boolean enabled;
    private final transient String url;
    private final transient HttpMethod method;
    private final transient Version version;
    private final transient Map<String, Document> customHeaders;
    private final transient List<String> events;
    private final transient int maxRetries;
    private final transient Instant lastTriggeredAt;
    private final transient String changeReason;
    private final transient String createdBy;
    private final transient Instant createdAt;
    private final transient String lastModifiedBy;
    private final transient Instant lastModifiedAt;

    private UpdateWebhookOutput(Builder builder) {
        this.name = builder.name;
        this.description = builder.description;
        this.enabled = builder.enabled;
        this.url = builder.url;
        this.method = builder.method;
        this.version = builder.version;
        this.customHeaders = builder.customHeaders == null ? null : Collections.unmodifiableMap(builder.customHeaders);
        this.events = Collections.unmodifiableList(builder.events);
        this.maxRetries = builder.maxRetries;
        this.lastTriggeredAt = builder.lastTriggeredAt;
        this.changeReason = builder.changeReason;
        this.createdBy = builder.createdBy;
        this.createdAt = builder.createdAt;
        this.lastModifiedBy = builder.lastModifiedBy;
        this.lastModifiedAt = builder.lastModifiedAt;
    }

    public String name() {
        return name;
    }

    public String description() {
        return description;
    }

    public boolean enabled() {
        return enabled;
    }

    public String url() {
        return url;
    }

    public HttpMethod method() {
        return method;
    }

    public Version version() {
        return version;
    }

    public Map<String, Document> customHeaders() {
        if (customHeaders == null) {
            return Collections.emptyMap();
        }
        return customHeaders;
    }

    public boolean hasCustomHeaders() {
        return customHeaders != null;
    }

    public List<String> events() {
        return events;
    }

    public boolean hasEvents() {
        return true;
    }

    public int maxRetries() {
        return maxRetries;
    }

    public Instant lastTriggeredAt() {
        return lastTriggeredAt;
    }

    public String changeReason() {
        return changeReason;
    }

    public String createdBy() {
        return createdBy;
    }

    public Instant createdAt() {
        return createdAt;
    }

    public String lastModifiedBy() {
        return lastModifiedBy;
    }

    public Instant lastModifiedAt() {
        return lastModifiedAt;
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
        UpdateWebhookOutput that = (UpdateWebhookOutput) other;
        return Objects.equals(this.name, that.name)
               && Objects.equals(this.description, that.description)
               && this.enabled == that.enabled
               && Objects.equals(this.url, that.url)
               && Objects.equals(this.method, that.method)
               && Objects.equals(this.version, that.version)
               && Objects.equals(this.customHeaders, that.customHeaders)
               && Objects.equals(this.events, that.events)
               && this.maxRetries == that.maxRetries
               && Objects.equals(this.lastTriggeredAt, that.lastTriggeredAt)
               && Objects.equals(this.changeReason, that.changeReason)
               && Objects.equals(this.createdBy, that.createdBy)
               && Objects.equals(this.createdAt, that.createdAt)
               && Objects.equals(this.lastModifiedBy, that.lastModifiedBy)
               && Objects.equals(this.lastModifiedAt, that.lastModifiedAt);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, description, enabled, url, method, version, customHeaders, events, maxRetries, lastTriggeredAt, changeReason, createdBy, createdAt, lastModifiedBy, lastModifiedAt);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_NAME, name);
        serializer.writeString($SCHEMA_DESCRIPTION, description);
        serializer.writeBoolean($SCHEMA_ENABLED, enabled);
        serializer.writeString($SCHEMA_URL, url);
        serializer.writeString($SCHEMA_METHOD, method.value());
        serializer.writeString($SCHEMA_VERSION, version.value());
        if (customHeaders != null) {
            serializer.writeMap($SCHEMA_CUSTOM_HEADERS, customHeaders, customHeaders.size(), SharedSerde.ObjectShapeSerializer.INSTANCE);
        }
        serializer.writeList($SCHEMA_EVENTS, events, events.size(), SharedSerde.EventsSerializer.INSTANCE);
        serializer.writeInteger($SCHEMA_MAX_RETRIES, maxRetries);
        if (lastTriggeredAt != null) {
            serializer.writeTimestamp($SCHEMA_LAST_TRIGGERED_AT, lastTriggeredAt);
        }
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
        serializer.writeString($SCHEMA_CREATED_BY, createdBy);
        serializer.writeTimestamp($SCHEMA_CREATED_AT, createdAt);
        serializer.writeString($SCHEMA_LAST_MODIFIED_BY, lastModifiedBy);
        serializer.writeTimestamp($SCHEMA_LAST_MODIFIED_AT, lastModifiedAt);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_NAME, member, name);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_ENABLED, member, enabled);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_URL, member, url);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_METHOD, member, method);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_VERSION, member, version);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_EVENTS, member, events);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_MAX_RETRIES, member, maxRetries);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 9 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, createdBy);
            case 10 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, createdAt);
            case 11 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, lastModifiedBy);
            case 12 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, lastModifiedAt);
            case 13 -> (T) SchemaUtils.validateSameMember($SCHEMA_CUSTOM_HEADERS, member, customHeaders);
            case 14 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_TRIGGERED_AT, member, lastTriggeredAt);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link UpdateWebhookOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.name(this.name);
        builder.description(this.description);
        builder.enabled(this.enabled);
        builder.url(this.url);
        builder.method(this.method);
        builder.version(this.version);
        builder.customHeaders(this.customHeaders);
        builder.events(this.events);
        builder.maxRetries(this.maxRetries);
        builder.lastTriggeredAt(this.lastTriggeredAt);
        builder.changeReason(this.changeReason);
        builder.createdBy(this.createdBy);
        builder.createdAt(this.createdAt);
        builder.lastModifiedBy(this.lastModifiedBy);
        builder.lastModifiedAt(this.lastModifiedAt);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link UpdateWebhookOutput}.
     */
    public static final class Builder implements ShapeBuilder<UpdateWebhookOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String name;
        private String description;
        private boolean enabled;
        private String url;
        private HttpMethod method;
        private Version version;
        private Map<String, Document> customHeaders;
        private List<String> events;
        private int maxRetries;
        private Instant lastTriggeredAt;
        private String changeReason;
        private String createdBy;
        private Instant createdAt;
        private String lastModifiedBy;
        private Instant lastModifiedAt;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder name(String name) {
            this.name = Objects.requireNonNull(name, "name cannot be null");
            tracker.setMember($SCHEMA_NAME);
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
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder enabled(boolean enabled) {
            this.enabled = enabled;
            tracker.setMember($SCHEMA_ENABLED);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder url(String url) {
            this.url = Objects.requireNonNull(url, "url cannot be null");
            tracker.setMember($SCHEMA_URL);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder method(HttpMethod method) {
            this.method = Objects.requireNonNull(method, "method cannot be null");
            tracker.setMember($SCHEMA_METHOD);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder version(Version version) {
            this.version = Objects.requireNonNull(version, "version cannot be null");
            tracker.setMember($SCHEMA_VERSION);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder customHeaders(Map<String, Document> customHeaders) {
            this.customHeaders = customHeaders;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder events(List<String> events) {
            this.events = Objects.requireNonNull(events, "events cannot be null");
            tracker.setMember($SCHEMA_EVENTS);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder maxRetries(int maxRetries) {
            this.maxRetries = maxRetries;
            tracker.setMember($SCHEMA_MAX_RETRIES);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder lastTriggeredAt(Instant lastTriggeredAt) {
            this.lastTriggeredAt = lastTriggeredAt;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder changeReason(String changeReason) {
            this.changeReason = Objects.requireNonNull(changeReason, "changeReason cannot be null");
            tracker.setMember($SCHEMA_CHANGE_REASON);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder createdBy(String createdBy) {
            this.createdBy = Objects.requireNonNull(createdBy, "createdBy cannot be null");
            tracker.setMember($SCHEMA_CREATED_BY);
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
        public Builder lastModifiedBy(String lastModifiedBy) {
            this.lastModifiedBy = Objects.requireNonNull(lastModifiedBy, "lastModifiedBy cannot be null");
            tracker.setMember($SCHEMA_LAST_MODIFIED_BY);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder lastModifiedAt(Instant lastModifiedAt) {
            this.lastModifiedAt = Objects.requireNonNull(lastModifiedAt, "lastModifiedAt cannot be null");
            tracker.setMember($SCHEMA_LAST_MODIFIED_AT);
            return this;
        }

        @Override
        public UpdateWebhookOutput build() {
            tracker.validate();
            return new UpdateWebhookOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> name((String) SchemaUtils.validateSameMember($SCHEMA_NAME, member, value));
                case 1 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 2 -> enabled((boolean) SchemaUtils.validateSameMember($SCHEMA_ENABLED, member, value));
                case 3 -> url((String) SchemaUtils.validateSameMember($SCHEMA_URL, member, value));
                case 4 -> method((HttpMethod) SchemaUtils.validateSameMember($SCHEMA_METHOD, member, value));
                case 5 -> version((Version) SchemaUtils.validateSameMember($SCHEMA_VERSION, member, value));
                case 6 -> events((List<String>) SchemaUtils.validateSameMember($SCHEMA_EVENTS, member, value));
                case 7 -> maxRetries((int) SchemaUtils.validateSameMember($SCHEMA_MAX_RETRIES, member, value));
                case 8 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 9 -> createdBy((String) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, value));
                case 10 -> createdAt((Instant) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, value));
                case 11 -> lastModifiedBy((String) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, value));
                case 12 -> lastModifiedAt((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, value));
                case 13 -> customHeaders((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_CUSTOM_HEADERS, member, value));
                case 14 -> lastTriggeredAt((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_TRIGGERED_AT, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<UpdateWebhookOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_NAME)) {
                name("");
            }
            if (!tracker.checkMember($SCHEMA_DESCRIPTION)) {
                description("");
            }
            if (!tracker.checkMember($SCHEMA_ENABLED)) {
                tracker.setMember($SCHEMA_ENABLED);
            }
            if (!tracker.checkMember($SCHEMA_URL)) {
                url("");
            }
            if (!tracker.checkMember($SCHEMA_METHOD)) {
                method(HttpMethod.unknown(""));
            }
            if (!tracker.checkMember($SCHEMA_VERSION)) {
                version(Version.unknown(""));
            }
            if (!tracker.checkMember($SCHEMA_EVENTS)) {
                events(Collections.emptyList());
            }
            if (!tracker.checkMember($SCHEMA_MAX_RETRIES)) {
                tracker.setMember($SCHEMA_MAX_RETRIES);
            }
            if (!tracker.checkMember($SCHEMA_CHANGE_REASON)) {
                changeReason("");
            }
            if (!tracker.checkMember($SCHEMA_CREATED_BY)) {
                createdBy("");
            }
            if (!tracker.checkMember($SCHEMA_CREATED_AT)) {
                createdAt(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED_BY)) {
                lastModifiedBy("");
            }
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED_AT)) {
                lastModifiedAt(Instant.EPOCH);
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
                    case 0 -> builder.name(de.readString(member));
                    case 1 -> builder.description(de.readString(member));
                    case 2 -> builder.enabled(de.readBoolean(member));
                    case 3 -> builder.url(de.readString(member));
                    case 4 -> builder.method(HttpMethod.builder().deserializeMember(de, member).build());
                    case 5 -> builder.version(Version.builder().deserializeMember(de, member).build());
                    case 6 -> builder.events(SharedSerde.deserializeEvents(member, de));
                    case 7 -> builder.maxRetries(de.readInteger(member));
                    case 8 -> builder.changeReason(de.readString(member));
                    case 9 -> builder.createdBy(de.readString(member));
                    case 10 -> builder.createdAt(de.readTimestamp(member));
                    case 11 -> builder.lastModifiedBy(de.readString(member));
                    case 12 -> builder.lastModifiedAt(de.readTimestamp(member));
                    case 13 -> builder.customHeaders(SharedSerde.deserializeObjectShape(member, de));
                    case 14 -> builder.lastTriggeredAt(de.readTimestamp(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

