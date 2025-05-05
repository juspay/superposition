
package io.juspay.superposition.model;

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
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.DefaultTrait;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpLabelTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class UpdateWebhookInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#UpdateWebhookInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-tenant"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new DefaultTrait(Node.from("juspay")),
                new RequiredTrait(),
                new HttpHeaderTrait("x-org-id"))
        .putMember("name", PreludeSchemas.STRING,
                new HttpLabelTrait(),
                new RequiredTrait())
        .putMember("description", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("enabled", PreludeSchemas.BOOLEAN,
                new RequiredTrait())
        .putMember("url", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("method", HttpMethod.$SCHEMA,
                new RequiredTrait())
        .putMember("version", Version.$SCHEMA)
        .putMember("custom_headers", SharedSchemas.OBJECT)
        .putMember("events", SharedSchemas.EVENTS,
                new RequiredTrait())
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_NAME = $SCHEMA.member("name");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_ENABLED = $SCHEMA.member("enabled");
    private static final Schema $SCHEMA_URL = $SCHEMA.member("url");
    private static final Schema $SCHEMA_METHOD = $SCHEMA.member("method");
    private static final Schema $SCHEMA_VERSION = $SCHEMA.member("version");
    private static final Schema $SCHEMA_CUSTOM_HEADERS = $SCHEMA.member("custom_headers");
    private static final Schema $SCHEMA_EVENTS = $SCHEMA.member("events");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient String name;
    private final transient String description;
    private final transient boolean enabled;
    private final transient String url;
    private final transient HttpMethod method;
    private final transient Version version;
    private final transient Map<String, Document> customHeaders;
    private final transient List<String> events;
    private final transient String changeReason;

    private UpdateWebhookInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.name = builder.name;
        this.description = builder.description;
        this.enabled = builder.enabled;
        this.url = builder.url;
        this.method = builder.method;
        this.version = builder.version;
        this.customHeaders = builder.customHeaders == null ? null : Collections.unmodifiableMap(builder.customHeaders);
        this.events = Collections.unmodifiableList(builder.events);
        this.changeReason = builder.changeReason;
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
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

    public String changeReason() {
        return changeReason;
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
        UpdateWebhookInput that = (UpdateWebhookInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.name, that.name)
               && Objects.equals(this.description, that.description)
               && this.enabled == that.enabled
               && Objects.equals(this.url, that.url)
               && Objects.equals(this.method, that.method)
               && Objects.equals(this.version, that.version)
               && Objects.equals(this.customHeaders, that.customHeaders)
               && Objects.equals(this.events, that.events)
               && Objects.equals(this.changeReason, that.changeReason);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, name, description, enabled, url, method, version, customHeaders, events, changeReason);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        serializer.writeString($SCHEMA_NAME, name);
        serializer.writeString($SCHEMA_DESCRIPTION, description);
        serializer.writeBoolean($SCHEMA_ENABLED, enabled);
        serializer.writeString($SCHEMA_URL, url);
        serializer.writeString($SCHEMA_METHOD, method.value());
        if (version != null) {
            serializer.writeString($SCHEMA_VERSION, version.value());
        }
        if (customHeaders != null) {
            serializer.writeMap($SCHEMA_CUSTOM_HEADERS, customHeaders, customHeaders.size(), SharedSerde.ObjectShapeSerializer.INSTANCE);
        }
        serializer.writeList($SCHEMA_EVENTS, events, events.size(), SharedSerde.EventsSerializer.INSTANCE);
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_NAME, member, name);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_ENABLED, member, enabled);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_URL, member, url);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_METHOD, member, method);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_EVENTS, member, events);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 9 -> (T) SchemaUtils.validateSameMember($SCHEMA_VERSION, member, version);
            case 10 -> (T) SchemaUtils.validateSameMember($SCHEMA_CUSTOM_HEADERS, member, customHeaders);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link UpdateWebhookInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.name(this.name);
        builder.description(this.description);
        builder.enabled(this.enabled);
        builder.url(this.url);
        builder.method(this.method);
        builder.version(this.version);
        builder.customHeaders(this.customHeaders);
        builder.events(this.events);
        builder.changeReason(this.changeReason);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link UpdateWebhookInput}.
     */
    public static final class Builder implements ShapeBuilder<UpdateWebhookInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId = ORG_ID_DEFAULT;
        private String name;
        private String description;
        private boolean enabled;
        private String url;
        private HttpMethod method;
        private Version version;
        private Map<String, Document> customHeaders;
        private List<String> events;
        private String changeReason;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder workspaceId(String workspaceId) {
            this.workspaceId = Objects.requireNonNull(workspaceId, "workspaceId cannot be null");
            tracker.setMember($SCHEMA_WORKSPACE_ID);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder orgId(String orgId) {
            this.orgId = Objects.requireNonNull(orgId, "orgId cannot be null");
            return this;
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
         * @return this builder.
         */
        public Builder version(Version version) {
            this.version = version;
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
        public Builder changeReason(String changeReason) {
            this.changeReason = Objects.requireNonNull(changeReason, "changeReason cannot be null");
            tracker.setMember($SCHEMA_CHANGE_REASON);
            return this;
        }

        @Override
        public UpdateWebhookInput build() {
            tracker.validate();
            return new UpdateWebhookInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> name((String) SchemaUtils.validateSameMember($SCHEMA_NAME, member, value));
                case 2 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 3 -> enabled((boolean) SchemaUtils.validateSameMember($SCHEMA_ENABLED, member, value));
                case 4 -> url((String) SchemaUtils.validateSameMember($SCHEMA_URL, member, value));
                case 5 -> method((HttpMethod) SchemaUtils.validateSameMember($SCHEMA_METHOD, member, value));
                case 6 -> events((List<String>) SchemaUtils.validateSameMember($SCHEMA_EVENTS, member, value));
                case 7 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 8 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 9 -> version((Version) SchemaUtils.validateSameMember($SCHEMA_VERSION, member, value));
                case 10 -> customHeaders((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_CUSTOM_HEADERS, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<UpdateWebhookInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
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
            if (!tracker.checkMember($SCHEMA_EVENTS)) {
                events(Collections.emptyList());
            }
            if (!tracker.checkMember($SCHEMA_CHANGE_REASON)) {
                changeReason("");
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
                    case 0 -> builder.workspaceId(de.readString(member));
                    case 1 -> builder.name(de.readString(member));
                    case 2 -> builder.description(de.readString(member));
                    case 3 -> builder.enabled(de.readBoolean(member));
                    case 4 -> builder.url(de.readString(member));
                    case 5 -> builder.method(HttpMethod.builder().deserializeMember(de, member).build());
                    case 6 -> builder.events(SharedSerde.deserializeEvents(member, de));
                    case 7 -> builder.changeReason(de.readString(member));
                    case 8 -> builder.orgId(de.readString(member));
                    case 9 -> builder.version(Version.builder().deserializeMember(de, member).build());
                    case 10 -> builder.customHeaders(SharedSerde.deserializeObjectShape(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

