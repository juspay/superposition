
package io.juspay.superposition.model;

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
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.DefaultTrait;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpQueryTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class ListWebhookInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListWebhookInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("count", PreludeSchemas.INTEGER,
                new HttpQueryTrait("count"))
        .putMember("page", PreludeSchemas.INTEGER,
                new HttpQueryTrait("page"))
        .putMember("all", PreludeSchemas.BOOLEAN,
                new HttpQueryTrait("all"))
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-tenant"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new DefaultTrait(Node.from("juspay")),
                new RequiredTrait(),
                new HttpHeaderTrait("x-org-id"))
        .build();

    private static final Schema $SCHEMA_COUNT = $SCHEMA.member("count");
    private static final Schema $SCHEMA_PAGE = $SCHEMA.member("page");
    private static final Schema $SCHEMA_ALL = $SCHEMA.member("all");
    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");

    private final transient Integer count;
    private final transient Integer page;
    private final transient Boolean all;
    private final transient String workspaceId;
    private final transient String orgId;

    private ListWebhookInput(Builder builder) {
        this.count = builder.count;
        this.page = builder.page;
        this.all = builder.all;
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
    }

    public Integer count() {
        return count;
    }

    public Integer page() {
        return page;
    }

    public Boolean all() {
        return all;
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
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
        ListWebhookInput that = (ListWebhookInput) other;
        return Objects.equals(this.count, that.count)
               && Objects.equals(this.page, that.page)
               && Objects.equals(this.all, that.all)
               && Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(count, page, all, workspaceId, orgId);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        if (count != null) {
            serializer.writeInteger($SCHEMA_COUNT, count);
        }
        if (page != null) {
            serializer.writeInteger($SCHEMA_PAGE, page);
        }
        if (all != null) {
            serializer.writeBoolean($SCHEMA_ALL, all);
        }
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_COUNT, member, count);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_PAGE, member, page);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_ALL, member, all);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ListWebhookInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.count(this.count);
        builder.page(this.page);
        builder.all(this.all);
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ListWebhookInput}.
     */
    public static final class Builder implements ShapeBuilder<ListWebhookInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private Integer count;
        private Integer page;
        private Boolean all;
        private String workspaceId;
        private String orgId = ORG_ID_DEFAULT;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * @return this builder.
         */
        public Builder count(int count) {
            this.count = count;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder page(int page) {
            this.page = page;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder all(boolean all) {
            this.all = all;
            return this;
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

        @Override
        public ListWebhookInput build() {
            tracker.validate();
            return new ListWebhookInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> count((int) SchemaUtils.validateSameMember($SCHEMA_COUNT, member, value));
                case 2 -> page((int) SchemaUtils.validateSameMember($SCHEMA_PAGE, member, value));
                case 3 -> all((boolean) SchemaUtils.validateSameMember($SCHEMA_ALL, member, value));
                case 4 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ListWebhookInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
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
                    case 1 -> builder.count(de.readInteger(member));
                    case 2 -> builder.page(de.readInteger(member));
                    case 3 -> builder.all(de.readBoolean(member));
                    case 4 -> builder.orgId(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

