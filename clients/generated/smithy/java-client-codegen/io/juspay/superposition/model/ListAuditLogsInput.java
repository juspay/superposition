
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
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.DefaultTrait;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpQueryTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class ListAuditLogsInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListAuditLogsInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-tenant"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new DefaultTrait(Node.from("juspay")),
                new RequiredTrait(),
                new HttpHeaderTrait("x-org-id"))
        .putMember("count", PreludeSchemas.INTEGER,
                new HttpQueryTrait("count"))
        .putMember("page", PreludeSchemas.INTEGER,
                new HttpQueryTrait("page"))
        .putMember("from_date", SharedSchemas.DATE_TIME,
                new HttpQueryTrait("from_date"))
        .putMember("to_date", SharedSchemas.DATE_TIME,
                new HttpQueryTrait("to_date"))
        .putMember("tables", PreludeSchemas.STRING,
                new HttpQueryTrait("table"))
        .putMember("action", PreludeSchemas.STRING,
                new HttpQueryTrait("action"))
        .putMember("username", PreludeSchemas.STRING,
                new HttpQueryTrait("username"))
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_COUNT = $SCHEMA.member("count");
    private static final Schema $SCHEMA_PAGE = $SCHEMA.member("page");
    private static final Schema $SCHEMA_FROM_DATE = $SCHEMA.member("from_date");
    private static final Schema $SCHEMA_TO_DATE = $SCHEMA.member("to_date");
    private static final Schema $SCHEMA_TABLES = $SCHEMA.member("tables");
    private static final Schema $SCHEMA_ACTION = $SCHEMA.member("action");
    private static final Schema $SCHEMA_USERNAME = $SCHEMA.member("username");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient Integer count;
    private final transient Integer page;
    private final transient Instant fromDate;
    private final transient Instant toDate;
    private final transient String tables;
    private final transient String action;
    private final transient String username;

    private ListAuditLogsInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.count = builder.count;
        this.page = builder.page;
        this.fromDate = builder.fromDate;
        this.toDate = builder.toDate;
        this.tables = builder.tables;
        this.action = builder.action;
        this.username = builder.username;
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
    }

    public Integer count() {
        return count;
    }

    public Integer page() {
        return page;
    }

    public Instant fromDate() {
        return fromDate;
    }

    public Instant toDate() {
        return toDate;
    }

    /**
     * Comma serparated list of tables.
     */
    public String tables() {
        return tables;
    }

    /**
     * Comma serparated list of actions.
     */
    public String action() {
        return action;
    }

    public String username() {
        return username;
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
        ListAuditLogsInput that = (ListAuditLogsInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.count, that.count)
               && Objects.equals(this.page, that.page)
               && Objects.equals(this.fromDate, that.fromDate)
               && Objects.equals(this.toDate, that.toDate)
               && Objects.equals(this.tables, that.tables)
               && Objects.equals(this.action, that.action)
               && Objects.equals(this.username, that.username);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, count, page, fromDate, toDate, tables, action, username);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        if (count != null) {
            serializer.writeInteger($SCHEMA_COUNT, count);
        }
        if (page != null) {
            serializer.writeInteger($SCHEMA_PAGE, page);
        }
        if (fromDate != null) {
            serializer.writeTimestamp($SCHEMA_FROM_DATE, fromDate);
        }
        if (toDate != null) {
            serializer.writeTimestamp($SCHEMA_TO_DATE, toDate);
        }
        if (tables != null) {
            serializer.writeString($SCHEMA_TABLES, tables);
        }
        if (action != null) {
            serializer.writeString($SCHEMA_ACTION, action);
        }
        if (username != null) {
            serializer.writeString($SCHEMA_USERNAME, username);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_COUNT, member, count);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_PAGE, member, page);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_FROM_DATE, member, fromDate);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_TO_DATE, member, toDate);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_TABLES, member, tables);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_ACTION, member, action);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_USERNAME, member, username);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ListAuditLogsInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.count(this.count);
        builder.page(this.page);
        builder.fromDate(this.fromDate);
        builder.toDate(this.toDate);
        builder.tables(this.tables);
        builder.action(this.action);
        builder.username(this.username);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ListAuditLogsInput}.
     */
    public static final class Builder implements ShapeBuilder<ListAuditLogsInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId = ORG_ID_DEFAULT;
        private Integer count;
        private Integer page;
        private Instant fromDate;
        private Instant toDate;
        private String tables;
        private String action;
        private String username;

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
        public Builder fromDate(Instant fromDate) {
            this.fromDate = fromDate;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder toDate(Instant toDate) {
            this.toDate = toDate;
            return this;
        }

        /**
         * Comma serparated list of tables.
         *
         * @return this builder.
         */
        public Builder tables(String tables) {
            this.tables = tables;
            return this;
        }

        /**
         * Comma serparated list of actions.
         *
         * @return this builder.
         */
        public Builder action(String action) {
            this.action = action;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder username(String username) {
            this.username = username;
            return this;
        }

        @Override
        public ListAuditLogsInput build() {
            tracker.validate();
            return new ListAuditLogsInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 2 -> count((int) SchemaUtils.validateSameMember($SCHEMA_COUNT, member, value));
                case 3 -> page((int) SchemaUtils.validateSameMember($SCHEMA_PAGE, member, value));
                case 4 -> fromDate((Instant) SchemaUtils.validateSameMember($SCHEMA_FROM_DATE, member, value));
                case 5 -> toDate((Instant) SchemaUtils.validateSameMember($SCHEMA_TO_DATE, member, value));
                case 6 -> tables((String) SchemaUtils.validateSameMember($SCHEMA_TABLES, member, value));
                case 7 -> action((String) SchemaUtils.validateSameMember($SCHEMA_ACTION, member, value));
                case 8 -> username((String) SchemaUtils.validateSameMember($SCHEMA_USERNAME, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ListAuditLogsInput> errorCorrection() {
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
                    case 1 -> builder.orgId(de.readString(member));
                    case 2 -> builder.count(de.readInteger(member));
                    case 3 -> builder.page(de.readInteger(member));
                    case 4 -> builder.fromDate(de.readTimestamp(member));
                    case 5 -> builder.toDate(de.readTimestamp(member));
                    case 6 -> builder.tables(de.readString(member));
                    case 7 -> builder.action(de.readString(member));
                    case 8 -> builder.username(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

