
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
public final class ListExperimentInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListExperimentInput");

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
        .putMember("all", PreludeSchemas.BOOLEAN,
                new HttpQueryTrait("all"))
        .putMember("status", ExperimentStatusType.$SCHEMA,
                new HttpQueryTrait("status"))
        .putMember("from_date", SharedSchemas.DATE_TIME,
                new HttpQueryTrait("from_date"))
        .putMember("to_date", SharedSchemas.DATE_TIME,
                new HttpQueryTrait("to_date"))
        .putMember("experiment_name", PreludeSchemas.STRING,
                new HttpQueryTrait("experiment_name"))
        .putMember("experiment_ids", PreludeSchemas.STRING,
                new HttpQueryTrait("experiment_ids"))
        .putMember("experiment_group_ids", PreludeSchemas.STRING,
                new HttpQueryTrait("experiment_group_ids"))
        .putMember("created_by", PreludeSchemas.STRING,
                new HttpQueryTrait("created_by"))
        .putMember("sort_on", ExperimentSortOn.$SCHEMA,
                new HttpQueryTrait("sort_on"))
        .putMember("sort_by", SortBy.$SCHEMA,
                new HttpQueryTrait("sort_by"))
        .putMember("global_experiments_only", PreludeSchemas.BOOLEAN,
                new HttpQueryTrait("global_experiments_only"))
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_PAGE = $SCHEMA.member("page");
    private static final Schema $SCHEMA_COUNT = $SCHEMA.member("count");
    private static final Schema $SCHEMA_ALL = $SCHEMA.member("all");
    private static final Schema $SCHEMA_STATUS = $SCHEMA.member("status");
    private static final Schema $SCHEMA_FROM_DATE = $SCHEMA.member("from_date");
    private static final Schema $SCHEMA_TO_DATE = $SCHEMA.member("to_date");
    private static final Schema $SCHEMA_EXPERIMENT_NAME = $SCHEMA.member("experiment_name");
    private static final Schema $SCHEMA_EXPERIMENT_IDS = $SCHEMA.member("experiment_ids");
    private static final Schema $SCHEMA_EXPERIMENT_GROUP_IDS = $SCHEMA.member("experiment_group_ids");
    private static final Schema $SCHEMA_CREATED_BY = $SCHEMA.member("created_by");
    private static final Schema $SCHEMA_SORT_ON = $SCHEMA.member("sort_on");
    private static final Schema $SCHEMA_SORT_BY = $SCHEMA.member("sort_by");
    private static final Schema $SCHEMA_GLOBAL_EXPERIMENTS_ONLY = $SCHEMA.member("global_experiments_only");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient Long page;
    private final transient Long count;
    private final transient Boolean all;
    private final transient ExperimentStatusType status;
    private final transient Instant fromDate;
    private final transient Instant toDate;
    private final transient String experimentName;
    private final transient String experimentIds;
    private final transient String experimentGroupIds;
    private final transient String createdBy;
    private final transient ExperimentSortOn sortOn;
    private final transient SortBy sortBy;
    private final transient Boolean globalExperimentsOnly;

    private ListExperimentInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.page = builder.page;
        this.count = builder.count;
        this.all = builder.all;
        this.status = builder.status;
        this.fromDate = builder.fromDate;
        this.toDate = builder.toDate;
        this.experimentName = builder.experimentName;
        this.experimentIds = builder.experimentIds;
        this.experimentGroupIds = builder.experimentGroupIds;
        this.createdBy = builder.createdBy;
        this.sortOn = builder.sortOn;
        this.sortBy = builder.sortBy;
        this.globalExperimentsOnly = builder.globalExperimentsOnly;
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

    public Boolean all() {
        return all;
    }

    public ExperimentStatusType status() {
        return status;
    }

    public Instant fromDate() {
        return fromDate;
    }

    public Instant toDate() {
        return toDate;
    }

    public String experimentName() {
        return experimentName;
    }

    public String experimentIds() {
        return experimentIds;
    }

    public String experimentGroupIds() {
        return experimentGroupIds;
    }

    public String createdBy() {
        return createdBy;
    }

    public ExperimentSortOn sortOn() {
        return sortOn;
    }

    public SortBy sortBy() {
        return sortBy;
    }

    public Boolean globalExperimentsOnly() {
        return globalExperimentsOnly;
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
        ListExperimentInput that = (ListExperimentInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.page, that.page)
               && Objects.equals(this.count, that.count)
               && Objects.equals(this.all, that.all)
               && Objects.equals(this.status, that.status)
               && Objects.equals(this.fromDate, that.fromDate)
               && Objects.equals(this.toDate, that.toDate)
               && Objects.equals(this.experimentName, that.experimentName)
               && Objects.equals(this.experimentIds, that.experimentIds)
               && Objects.equals(this.experimentGroupIds, that.experimentGroupIds)
               && Objects.equals(this.createdBy, that.createdBy)
               && Objects.equals(this.sortOn, that.sortOn)
               && Objects.equals(this.sortBy, that.sortBy)
               && Objects.equals(this.globalExperimentsOnly, that.globalExperimentsOnly);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, page, count, all, status, fromDate, toDate, experimentName, experimentIds, experimentGroupIds, createdBy, sortOn, sortBy, globalExperimentsOnly);
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
        if (all != null) {
            serializer.writeBoolean($SCHEMA_ALL, all);
        }
        if (status != null) {
            serializer.writeString($SCHEMA_STATUS, status.value());
        }
        if (fromDate != null) {
            serializer.writeTimestamp($SCHEMA_FROM_DATE, fromDate);
        }
        if (toDate != null) {
            serializer.writeTimestamp($SCHEMA_TO_DATE, toDate);
        }
        if (experimentName != null) {
            serializer.writeString($SCHEMA_EXPERIMENT_NAME, experimentName);
        }
        if (experimentIds != null) {
            serializer.writeString($SCHEMA_EXPERIMENT_IDS, experimentIds);
        }
        if (experimentGroupIds != null) {
            serializer.writeString($SCHEMA_EXPERIMENT_GROUP_IDS, experimentGroupIds);
        }
        if (createdBy != null) {
            serializer.writeString($SCHEMA_CREATED_BY, createdBy);
        }
        if (sortOn != null) {
            serializer.writeString($SCHEMA_SORT_ON, sortOn.value());
        }
        if (sortBy != null) {
            serializer.writeString($SCHEMA_SORT_BY, sortBy.value());
        }
        if (globalExperimentsOnly != null) {
            serializer.writeBoolean($SCHEMA_GLOBAL_EXPERIMENTS_ONLY, globalExperimentsOnly);
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
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_ALL, member, all);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_STATUS, member, status);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_FROM_DATE, member, fromDate);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_TO_DATE, member, toDate);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_NAME, member, experimentName);
            case 9 -> (T) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_IDS, member, experimentIds);
            case 10 -> (T) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_GROUP_IDS, member, experimentGroupIds);
            case 11 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, createdBy);
            case 12 -> (T) SchemaUtils.validateSameMember($SCHEMA_SORT_ON, member, sortOn);
            case 13 -> (T) SchemaUtils.validateSameMember($SCHEMA_SORT_BY, member, sortBy);
            case 14 -> (T) SchemaUtils.validateSameMember($SCHEMA_GLOBAL_EXPERIMENTS_ONLY, member, globalExperimentsOnly);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ListExperimentInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.page(this.page);
        builder.count(this.count);
        builder.all(this.all);
        builder.status(this.status);
        builder.fromDate(this.fromDate);
        builder.toDate(this.toDate);
        builder.experimentName(this.experimentName);
        builder.experimentIds(this.experimentIds);
        builder.experimentGroupIds(this.experimentGroupIds);
        builder.createdBy(this.createdBy);
        builder.sortOn(this.sortOn);
        builder.sortBy(this.sortBy);
        builder.globalExperimentsOnly(this.globalExperimentsOnly);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ListExperimentInput}.
     */
    public static final class Builder implements ShapeBuilder<ListExperimentInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId = ORG_ID_DEFAULT;
        private Long page;
        private Long count;
        private Boolean all;
        private ExperimentStatusType status;
        private Instant fromDate;
        private Instant toDate;
        private String experimentName;
        private String experimentIds;
        private String experimentGroupIds;
        private String createdBy;
        private ExperimentSortOn sortOn;
        private SortBy sortBy;
        private Boolean globalExperimentsOnly;

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
         * @return this builder.
         */
        public Builder all(boolean all) {
            this.all = all;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder status(ExperimentStatusType status) {
            this.status = status;
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
         * @return this builder.
         */
        public Builder experimentName(String experimentName) {
            this.experimentName = experimentName;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder experimentIds(String experimentIds) {
            this.experimentIds = experimentIds;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder experimentGroupIds(String experimentGroupIds) {
            this.experimentGroupIds = experimentGroupIds;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder createdBy(String createdBy) {
            this.createdBy = createdBy;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder sortOn(ExperimentSortOn sortOn) {
            this.sortOn = sortOn;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder sortBy(SortBy sortBy) {
            this.sortBy = sortBy;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder globalExperimentsOnly(boolean globalExperimentsOnly) {
            this.globalExperimentsOnly = globalExperimentsOnly;
            return this;
        }

        @Override
        public ListExperimentInput build() {
            tracker.validate();
            return new ListExperimentInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 2 -> page((long) SchemaUtils.validateSameMember($SCHEMA_PAGE, member, value));
                case 3 -> count((long) SchemaUtils.validateSameMember($SCHEMA_COUNT, member, value));
                case 4 -> all((boolean) SchemaUtils.validateSameMember($SCHEMA_ALL, member, value));
                case 5 -> status((ExperimentStatusType) SchemaUtils.validateSameMember($SCHEMA_STATUS, member, value));
                case 6 -> fromDate((Instant) SchemaUtils.validateSameMember($SCHEMA_FROM_DATE, member, value));
                case 7 -> toDate((Instant) SchemaUtils.validateSameMember($SCHEMA_TO_DATE, member, value));
                case 8 -> experimentName((String) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_NAME, member, value));
                case 9 -> experimentIds((String) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_IDS, member, value));
                case 10 -> experimentGroupIds((String) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_GROUP_IDS, member, value));
                case 11 -> createdBy((String) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, value));
                case 12 -> sortOn((ExperimentSortOn) SchemaUtils.validateSameMember($SCHEMA_SORT_ON, member, value));
                case 13 -> sortBy((SortBy) SchemaUtils.validateSameMember($SCHEMA_SORT_BY, member, value));
                case 14 -> globalExperimentsOnly((boolean) SchemaUtils.validateSameMember($SCHEMA_GLOBAL_EXPERIMENTS_ONLY, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ListExperimentInput> errorCorrection() {
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
                    case 4 -> builder.all(de.readBoolean(member));
                    case 5 -> builder.status(ExperimentStatusType.builder().deserializeMember(de, member).build());
                    case 6 -> builder.fromDate(de.readTimestamp(member));
                    case 7 -> builder.toDate(de.readTimestamp(member));
                    case 8 -> builder.experimentName(de.readString(member));
                    case 9 -> builder.experimentIds(de.readString(member));
                    case 10 -> builder.experimentGroupIds(de.readString(member));
                    case 11 -> builder.createdBy(de.readString(member));
                    case 12 -> builder.sortOn(ExperimentSortOn.builder().deserializeMember(de, member).build());
                    case 13 -> builder.sortBy(SortBy.builder().deserializeMember(de, member).build());
                    case 14 -> builder.globalExperimentsOnly(de.readBoolean(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

