
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
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpQueryTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class ListExperimentInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListExperimentInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("count", PreludeSchemas.INTEGER,
                new HttpQueryTrait("count"))
        .putMember("page", PreludeSchemas.INTEGER,
                new HttpQueryTrait("page"))
        .putMember("all", PreludeSchemas.BOOLEAN,
                new HttpQueryTrait("all"))
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-workspace"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-org-id"),
                new RequiredTrait())
        .putMember("status", SharedSchemas.EXPERIMENT_STATUS_TYPE_LIST,
                new HttpQueryTrait("status"))
        .putMember("from_date", SharedSchemas.DATE_TIME,
                new HttpQueryTrait("from_date"))
        .putMember("to_date", SharedSchemas.DATE_TIME,
                new HttpQueryTrait("to_date"))
        .putMember("experiment_name", PreludeSchemas.STRING,
                new HttpQueryTrait("experiment_name"))
        .putMember("experiment_ids", SharedSchemas.STRING_LIST,
                new HttpQueryTrait("experiment_ids"))
        .putMember("experiment_group_ids", SharedSchemas.STRING_LIST,
                new HttpQueryTrait("experiment_group_ids"))
        .putMember("created_by", SharedSchemas.STRING_LIST,
                new HttpQueryTrait("created_by"))
        .putMember("sort_on", ExperimentSortOn.$SCHEMA,
                new HttpQueryTrait("sort_on"))
        .putMember("sort_by", SortBy.$SCHEMA,
                new HttpQueryTrait("sort_by"))
        .putMember("global_experiments_only", PreludeSchemas.BOOLEAN,
                new HttpQueryTrait("global_experiments_only"))
        .putMember("dimension_match_strategy", DimensionMatchStrategy.$SCHEMA,
                new HttpQueryTrait("dimension_match_strategy"))
        .build();

    private static final Schema $SCHEMA_COUNT = $SCHEMA.member("count");
    private static final Schema $SCHEMA_PAGE = $SCHEMA.member("page");
    private static final Schema $SCHEMA_ALL = $SCHEMA.member("all");
    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
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
    private static final Schema $SCHEMA_DIMENSION_MATCH_STRATEGY = $SCHEMA.member("dimension_match_strategy");

    private final transient Integer count;
    private final transient Integer page;
    private final transient Boolean all;
    private final transient String workspaceId;
    private final transient String orgId;
    private final transient List<ExperimentStatusType> status;
    private final transient Instant fromDate;
    private final transient Instant toDate;
    private final transient String experimentName;
    private final transient List<String> experimentIds;
    private final transient List<String> experimentGroupIds;
    private final transient List<String> createdBy;
    private final transient ExperimentSortOn sortOn;
    private final transient SortBy sortBy;
    private final transient Boolean globalExperimentsOnly;
    private final transient DimensionMatchStrategy dimensionMatchStrategy;

    private ListExperimentInput(Builder builder) {
        this.count = builder.count;
        this.page = builder.page;
        this.all = builder.all;
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.status = builder.status == null ? null : Collections.unmodifiableList(builder.status);
        this.fromDate = builder.fromDate;
        this.toDate = builder.toDate;
        this.experimentName = builder.experimentName;
        this.experimentIds = builder.experimentIds == null ? null : Collections.unmodifiableList(builder.experimentIds);
        this.experimentGroupIds = builder.experimentGroupIds == null ? null : Collections.unmodifiableList(builder.experimentGroupIds);
        this.createdBy = builder.createdBy == null ? null : Collections.unmodifiableList(builder.createdBy);
        this.sortOn = builder.sortOn;
        this.sortBy = builder.sortBy;
        this.globalExperimentsOnly = builder.globalExperimentsOnly;
        this.dimensionMatchStrategy = builder.dimensionMatchStrategy;
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

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
    }

    public List<ExperimentStatusType> status() {
        if (status == null) {
            return Collections.emptyList();
        }
        return status;
    }

    public boolean hasStatus() {
        return status != null;
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

    public List<String> experimentIds() {
        if (experimentIds == null) {
            return Collections.emptyList();
        }
        return experimentIds;
    }

    public boolean hasExperimentIds() {
        return experimentIds != null;
    }

    public List<String> experimentGroupIds() {
        if (experimentGroupIds == null) {
            return Collections.emptyList();
        }
        return experimentGroupIds;
    }

    public boolean hasExperimentGroupIds() {
        return experimentGroupIds != null;
    }

    public List<String> createdBy() {
        if (createdBy == null) {
            return Collections.emptyList();
        }
        return createdBy;
    }

    public boolean hasCreatedBy() {
        return createdBy != null;
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

    public DimensionMatchStrategy dimensionMatchStrategy() {
        return dimensionMatchStrategy;
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
        return Objects.equals(this.count, that.count)
               && Objects.equals(this.page, that.page)
               && Objects.equals(this.all, that.all)
               && Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.status, that.status)
               && Objects.equals(this.fromDate, that.fromDate)
               && Objects.equals(this.toDate, that.toDate)
               && Objects.equals(this.experimentName, that.experimentName)
               && Objects.equals(this.experimentIds, that.experimentIds)
               && Objects.equals(this.experimentGroupIds, that.experimentGroupIds)
               && Objects.equals(this.createdBy, that.createdBy)
               && Objects.equals(this.sortOn, that.sortOn)
               && Objects.equals(this.sortBy, that.sortBy)
               && Objects.equals(this.globalExperimentsOnly, that.globalExperimentsOnly)
               && Objects.equals(this.dimensionMatchStrategy, that.dimensionMatchStrategy);
    }

    @Override
    public int hashCode() {
        return Objects.hash(count, page, all, workspaceId, orgId, status, fromDate, toDate, experimentName, experimentIds, experimentGroupIds, createdBy, sortOn, sortBy, globalExperimentsOnly, dimensionMatchStrategy);
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
        if (status != null) {
            serializer.writeList($SCHEMA_STATUS, status, status.size(), SharedSerde.ExperimentStatusTypeListSerializer.INSTANCE);
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
            serializer.writeList($SCHEMA_EXPERIMENT_IDS, experimentIds, experimentIds.size(), SharedSerde.StringListSerializer.INSTANCE);
        }
        if (experimentGroupIds != null) {
            serializer.writeList($SCHEMA_EXPERIMENT_GROUP_IDS, experimentGroupIds, experimentGroupIds.size(), SharedSerde.StringListSerializer.INSTANCE);
        }
        if (createdBy != null) {
            serializer.writeList($SCHEMA_CREATED_BY, createdBy, createdBy.size(), SharedSerde.StringListSerializer.INSTANCE);
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
        if (dimensionMatchStrategy != null) {
            serializer.writeString($SCHEMA_DIMENSION_MATCH_STRATEGY, dimensionMatchStrategy.value());
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
            case 15 -> (T) SchemaUtils.validateSameMember($SCHEMA_DIMENSION_MATCH_STRATEGY, member, dimensionMatchStrategy);
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
        builder.count(this.count);
        builder.page(this.page);
        builder.all(this.all);
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
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
        builder.dimensionMatchStrategy(this.dimensionMatchStrategy);
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
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private Integer count;
        private Integer page;
        private Boolean all;
        private String workspaceId;
        private String orgId;
        private List<ExperimentStatusType> status;
        private Instant fromDate;
        private Instant toDate;
        private String experimentName;
        private List<String> experimentIds;
        private List<String> experimentGroupIds;
        private List<String> createdBy;
        private ExperimentSortOn sortOn;
        private SortBy sortBy;
        private Boolean globalExperimentsOnly;
        private DimensionMatchStrategy dimensionMatchStrategy;

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
            tracker.setMember($SCHEMA_ORG_ID);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder status(List<ExperimentStatusType> status) {
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
        public Builder experimentIds(List<String> experimentIds) {
            this.experimentIds = experimentIds;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder experimentGroupIds(List<String> experimentGroupIds) {
            this.experimentGroupIds = experimentGroupIds;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder createdBy(List<String> createdBy) {
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

        /**
         * @return this builder.
         */
        public Builder dimensionMatchStrategy(DimensionMatchStrategy dimensionMatchStrategy) {
            this.dimensionMatchStrategy = dimensionMatchStrategy;
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
                case 2 -> count((int) SchemaUtils.validateSameMember($SCHEMA_COUNT, member, value));
                case 3 -> page((int) SchemaUtils.validateSameMember($SCHEMA_PAGE, member, value));
                case 4 -> all((boolean) SchemaUtils.validateSameMember($SCHEMA_ALL, member, value));
                case 5 -> status((List<ExperimentStatusType>) SchemaUtils.validateSameMember($SCHEMA_STATUS, member, value));
                case 6 -> fromDate((Instant) SchemaUtils.validateSameMember($SCHEMA_FROM_DATE, member, value));
                case 7 -> toDate((Instant) SchemaUtils.validateSameMember($SCHEMA_TO_DATE, member, value));
                case 8 -> experimentName((String) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_NAME, member, value));
                case 9 -> experimentIds((List<String>) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_IDS, member, value));
                case 10 -> experimentGroupIds((List<String>) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_GROUP_IDS, member, value));
                case 11 -> createdBy((List<String>) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, value));
                case 12 -> sortOn((ExperimentSortOn) SchemaUtils.validateSameMember($SCHEMA_SORT_ON, member, value));
                case 13 -> sortBy((SortBy) SchemaUtils.validateSameMember($SCHEMA_SORT_BY, member, value));
                case 14 -> globalExperimentsOnly((boolean) SchemaUtils.validateSameMember($SCHEMA_GLOBAL_EXPERIMENTS_ONLY, member, value));
                case 15 -> dimensionMatchStrategy((DimensionMatchStrategy) SchemaUtils.validateSameMember($SCHEMA_DIMENSION_MATCH_STRATEGY, member, value));
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
                    case 0 -> builder.workspaceId(de.readString(member));
                    case 1 -> builder.orgId(de.readString(member));
                    case 2 -> builder.count(de.readInteger(member));
                    case 3 -> builder.page(de.readInteger(member));
                    case 4 -> builder.all(de.readBoolean(member));
                    case 5 -> builder.status(SharedSerde.deserializeExperimentStatusTypeList(member, de));
                    case 6 -> builder.fromDate(de.readTimestamp(member));
                    case 7 -> builder.toDate(de.readTimestamp(member));
                    case 8 -> builder.experimentName(de.readString(member));
                    case 9 -> builder.experimentIds(SharedSerde.deserializeStringList(member, de));
                    case 10 -> builder.experimentGroupIds(SharedSerde.deserializeStringList(member, de));
                    case 11 -> builder.createdBy(SharedSerde.deserializeStringList(member, de));
                    case 12 -> builder.sortOn(ExperimentSortOn.builder().deserializeMember(de, member).build());
                    case 13 -> builder.sortBy(SortBy.builder().deserializeMember(de, member).build());
                    case 14 -> builder.globalExperimentsOnly(de.readBoolean(member));
                    case 15 -> builder.dimensionMatchStrategy(DimensionMatchStrategy.builder().deserializeMember(de, member).build());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

