
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
public final class ListExperimentGroupsInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListExperimentGroupsInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-tenant"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new DefaultTrait(Node.from("juspay")),
                new RequiredTrait(),
                new HttpHeaderTrait("x-org-id"))
        .putMember("page", PreludeSchemas.LONG,
                new HttpQueryTrait("page"))
        .putMember("count", PreludeSchemas.LONG,
                new HttpQueryTrait("count"))
        .putMember("name", PreludeSchemas.STRING,
                new HttpQueryTrait("name"))
        .putMember("created_by", PreludeSchemas.STRING,
                new HttpQueryTrait("created_by"))
        .putMember("last_modified_by", PreludeSchemas.STRING,
                new HttpQueryTrait("last_modified_by"))
        .putMember("sort_on", ExperimentGroupSortOn.$SCHEMA,
                new HttpQueryTrait("sort_on"))
        .putMember("sort_by", SortBy.$SCHEMA,
                new HttpQueryTrait("sort_by"))
        .putMember("all", PreludeSchemas.BOOLEAN,
                new HttpQueryTrait("all"))
        .putMember("group_type", GroupType.$SCHEMA,
                new HttpQueryTrait("group_type"))
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_PAGE = $SCHEMA.member("page");
    private static final Schema $SCHEMA_COUNT = $SCHEMA.member("count");
    private static final Schema $SCHEMA_NAME = $SCHEMA.member("name");
    private static final Schema $SCHEMA_CREATED_BY = $SCHEMA.member("created_by");
    private static final Schema $SCHEMA_LAST_MODIFIED_BY = $SCHEMA.member("last_modified_by");
    private static final Schema $SCHEMA_SORT_ON = $SCHEMA.member("sort_on");
    private static final Schema $SCHEMA_SORT_BY = $SCHEMA.member("sort_by");
    private static final Schema $SCHEMA_ALL = $SCHEMA.member("all");
    private static final Schema $SCHEMA_GROUP_TYPE = $SCHEMA.member("group_type");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient Long page;
    private final transient Long count;
    private final transient String name;
    private final transient String createdBy;
    private final transient String lastModifiedBy;
    private final transient ExperimentGroupSortOn sortOn;
    private final transient SortBy sortBy;
    private final transient Boolean all;
    private final transient GroupType groupType;

    private ListExperimentGroupsInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.page = builder.page;
        this.count = builder.count;
        this.name = builder.name;
        this.createdBy = builder.createdBy;
        this.lastModifiedBy = builder.lastModifiedBy;
        this.sortOn = builder.sortOn;
        this.sortBy = builder.sortBy;
        this.all = builder.all;
        this.groupType = builder.groupType;
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
    }

    public Long page() {
        return page;
    }

    public Long count() {
        return count;
    }

    /**
     * Filter by experiment group name (exact match or substring, depending on backend implementation).
     */
    public String name() {
        return name;
    }

    /**
     * Filter by the user who created the experiment group.
     */
    public String createdBy() {
        return createdBy;
    }

    /**
     * Filter by the user who last modified the experiment group.
     */
    public String lastModifiedBy() {
        return lastModifiedBy;
    }

    /**
     * Field to sort the results by.
     */
    public ExperimentGroupSortOn sortOn() {
        return sortOn;
    }

    /**
     * Sort order (ascending or descending).
     */
    public SortBy sortBy() {
        return sortBy;
    }

    /**
     * If true, returns all experiment groups, ignoring pagination parameters page and count.
     */
    public Boolean all() {
        return all;
    }

    /**
     * Filter by the type of group (USER_CREATED or SYSTEM_GENERATED).
     */
    public GroupType groupType() {
        return groupType;
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
        ListExperimentGroupsInput that = (ListExperimentGroupsInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.page, that.page)
               && Objects.equals(this.count, that.count)
               && Objects.equals(this.name, that.name)
               && Objects.equals(this.createdBy, that.createdBy)
               && Objects.equals(this.lastModifiedBy, that.lastModifiedBy)
               && Objects.equals(this.sortOn, that.sortOn)
               && Objects.equals(this.sortBy, that.sortBy)
               && Objects.equals(this.all, that.all)
               && Objects.equals(this.groupType, that.groupType);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, page, count, name, createdBy, lastModifiedBy, sortOn, sortBy, all, groupType);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        if (page != null) {
            serializer.writeLong($SCHEMA_PAGE, page);
        }
        if (count != null) {
            serializer.writeLong($SCHEMA_COUNT, count);
        }
        if (name != null) {
            serializer.writeString($SCHEMA_NAME, name);
        }
        if (createdBy != null) {
            serializer.writeString($SCHEMA_CREATED_BY, createdBy);
        }
        if (lastModifiedBy != null) {
            serializer.writeString($SCHEMA_LAST_MODIFIED_BY, lastModifiedBy);
        }
        if (sortOn != null) {
            serializer.writeString($SCHEMA_SORT_ON, sortOn.value());
        }
        if (sortBy != null) {
            serializer.writeString($SCHEMA_SORT_BY, sortBy.value());
        }
        if (all != null) {
            serializer.writeBoolean($SCHEMA_ALL, all);
        }
        if (groupType != null) {
            serializer.writeString($SCHEMA_GROUP_TYPE, groupType.value());
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_PAGE, member, page);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_COUNT, member, count);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_NAME, member, name);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, createdBy);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, lastModifiedBy);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_SORT_ON, member, sortOn);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_SORT_BY, member, sortBy);
            case 9 -> (T) SchemaUtils.validateSameMember($SCHEMA_ALL, member, all);
            case 10 -> (T) SchemaUtils.validateSameMember($SCHEMA_GROUP_TYPE, member, groupType);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ListExperimentGroupsInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.page(this.page);
        builder.count(this.count);
        builder.name(this.name);
        builder.createdBy(this.createdBy);
        builder.lastModifiedBy(this.lastModifiedBy);
        builder.sortOn(this.sortOn);
        builder.sortBy(this.sortBy);
        builder.all(this.all);
        builder.groupType(this.groupType);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ListExperimentGroupsInput}.
     */
    public static final class Builder implements ShapeBuilder<ListExperimentGroupsInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId = ORG_ID_DEFAULT;
        private Long page;
        private Long count;
        private String name;
        private String createdBy;
        private String lastModifiedBy;
        private ExperimentGroupSortOn sortOn;
        private SortBy sortBy;
        private Boolean all;
        private GroupType groupType;

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
        public Builder page(long page) {
            this.page = page;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder count(long count) {
            this.count = count;
            return this;
        }

        /**
         * Filter by experiment group name (exact match or substring, depending on backend implementation).
         *
         * @return this builder.
         */
        public Builder name(String name) {
            this.name = name;
            return this;
        }

        /**
         * Filter by the user who created the experiment group.
         *
         * @return this builder.
         */
        public Builder createdBy(String createdBy) {
            this.createdBy = createdBy;
            return this;
        }

        /**
         * Filter by the user who last modified the experiment group.
         *
         * @return this builder.
         */
        public Builder lastModifiedBy(String lastModifiedBy) {
            this.lastModifiedBy = lastModifiedBy;
            return this;
        }

        /**
         * Field to sort the results by.
         *
         * @return this builder.
         */
        public Builder sortOn(ExperimentGroupSortOn sortOn) {
            this.sortOn = sortOn;
            return this;
        }

        /**
         * Sort order (ascending or descending).
         *
         * @return this builder.
         */
        public Builder sortBy(SortBy sortBy) {
            this.sortBy = sortBy;
            return this;
        }

        /**
         * If true, returns all experiment groups, ignoring pagination parameters page and count.
         *
         * @return this builder.
         */
        public Builder all(boolean all) {
            this.all = all;
            return this;
        }

        /**
         * Filter by the type of group (USER_CREATED or SYSTEM_GENERATED).
         *
         * @return this builder.
         */
        public Builder groupType(GroupType groupType) {
            this.groupType = groupType;
            return this;
        }

        @Override
        public ListExperimentGroupsInput build() {
            tracker.validate();
            return new ListExperimentGroupsInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 2 -> page((long) SchemaUtils.validateSameMember($SCHEMA_PAGE, member, value));
                case 3 -> count((long) SchemaUtils.validateSameMember($SCHEMA_COUNT, member, value));
                case 4 -> name((String) SchemaUtils.validateSameMember($SCHEMA_NAME, member, value));
                case 5 -> createdBy((String) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, value));
                case 6 -> lastModifiedBy((String) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, value));
                case 7 -> sortOn((ExperimentGroupSortOn) SchemaUtils.validateSameMember($SCHEMA_SORT_ON, member, value));
                case 8 -> sortBy((SortBy) SchemaUtils.validateSameMember($SCHEMA_SORT_BY, member, value));
                case 9 -> all((boolean) SchemaUtils.validateSameMember($SCHEMA_ALL, member, value));
                case 10 -> groupType((GroupType) SchemaUtils.validateSameMember($SCHEMA_GROUP_TYPE, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ListExperimentGroupsInput> errorCorrection() {
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
                    case 2 -> builder.page(de.readLong(member));
                    case 3 -> builder.count(de.readLong(member));
                    case 4 -> builder.name(de.readString(member));
                    case 5 -> builder.createdBy(de.readString(member));
                    case 6 -> builder.lastModifiedBy(de.readString(member));
                    case 7 -> builder.sortOn(ExperimentGroupSortOn.builder().deserializeMember(de, member).build());
                    case 8 -> builder.sortBy(SortBy.builder().deserializeMember(de, member).build());
                    case 9 -> builder.all(de.readBoolean(member));
                    case 10 -> builder.groupType(GroupType.builder().deserializeMember(de, member).build());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

