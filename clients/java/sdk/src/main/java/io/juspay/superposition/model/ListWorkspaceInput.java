
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
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpQueryTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class ListWorkspaceInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListWorkspaceInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("count", PreludeSchemas.INTEGER,
                new HttpQueryTrait("count"))
        .putMember("page", PreludeSchemas.INTEGER,
                new HttpQueryTrait("page"))
        .putMember("all", PreludeSchemas.BOOLEAN,
                new HttpQueryTrait("all"))
        .putMember("org_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-org-id"),
                new RequiredTrait())
        .putMember("user_agent", PreludeSchemas.STRING,
                new HttpHeaderTrait("user-agent"))
        .build();

    private static final Schema $SCHEMA_COUNT = $SCHEMA.member("count");
    private static final Schema $SCHEMA_PAGE = $SCHEMA.member("page");
    private static final Schema $SCHEMA_ALL = $SCHEMA.member("all");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_USER_AGENT = $SCHEMA.member("user_agent");

    private final transient Integer count;
    private final transient Integer page;
    private final transient Boolean all;
    private final transient String orgId;
    private final transient String userAgent;

    private ListWorkspaceInput(Builder builder) {
        this.count = builder.count;
        this.page = builder.page;
        this.all = builder.all;
        this.orgId = builder.orgId;
        this.userAgent = builder.userAgent;
    }

    /**
     * Number of items to be returned in each page.
     */
    public Integer count() {
        return count;
    }

    /**
     * Page number to retrieve, starting from 1.
     */
    public Integer page() {
        return page;
    }

    /**
     * If true, returns all requested items, ignoring pagination parameters page and count.
     */
    public Boolean all() {
        return all;
    }

    public String orgId() {
        return orgId;
    }

    public String userAgent() {
        return userAgent;
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
        ListWorkspaceInput that = (ListWorkspaceInput) other;
        return Objects.equals(this.count, that.count)
               && Objects.equals(this.page, that.page)
               && Objects.equals(this.all, that.all)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.userAgent, that.userAgent);
    }

    @Override
    public int hashCode() {
        return Objects.hash(count, page, all, orgId, userAgent);
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
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        if (userAgent != null) {
            serializer.writeString($SCHEMA_USER_AGENT, userAgent);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_COUNT, member, count);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_PAGE, member, page);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_ALL, member, all);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_USER_AGENT, member, userAgent);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ListWorkspaceInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.count(this.count);
        builder.page(this.page);
        builder.all(this.all);
        builder.orgId(this.orgId);
        builder.userAgent(this.userAgent);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ListWorkspaceInput}.
     */
    public static final class Builder implements ShapeBuilder<ListWorkspaceInput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private Integer count;
        private Integer page;
        private Boolean all;
        private String orgId;
        private String userAgent;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * Number of items to be returned in each page.
         *
         * @return this builder.
         */
        public Builder count(int count) {
            this.count = count;
            return this;
        }

        /**
         * Page number to retrieve, starting from 1.
         *
         * @return this builder.
         */
        public Builder page(int page) {
            this.page = page;
            return this;
        }

        /**
         * If true, returns all requested items, ignoring pagination parameters page and count.
         *
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
        public Builder orgId(String orgId) {
            this.orgId = Objects.requireNonNull(orgId, "orgId cannot be null");
            tracker.setMember($SCHEMA_ORG_ID);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder userAgent(String userAgent) {
            this.userAgent = userAgent;
            return this;
        }

        @Override
        public ListWorkspaceInput build() {
            tracker.validate();
            return new ListWorkspaceInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 1 -> count((int) SchemaUtils.validateSameMember($SCHEMA_COUNT, member, value));
                case 2 -> page((int) SchemaUtils.validateSameMember($SCHEMA_PAGE, member, value));
                case 3 -> all((boolean) SchemaUtils.validateSameMember($SCHEMA_ALL, member, value));
                case 4 -> userAgent((String) SchemaUtils.validateSameMember($SCHEMA_USER_AGENT, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ListWorkspaceInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_ORG_ID)) {
                orgId("");
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
                    case 0 -> builder.orgId(de.readString(member));
                    case 1 -> builder.count(de.readInteger(member));
                    case 2 -> builder.page(de.readInteger(member));
                    case 3 -> builder.all(de.readBoolean(member));
                    case 4 -> builder.userAgent(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

