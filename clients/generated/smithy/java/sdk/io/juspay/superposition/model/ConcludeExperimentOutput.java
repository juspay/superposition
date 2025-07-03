
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
public final class ConcludeExperimentOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ExperimentResponse");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("created_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("created_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("last_modified", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("name", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("experiment_type", ExperimentType.$SCHEMA,
                new RequiredTrait())
        .putMember("override_keys", SharedSchemas.LIST_OVERRIDE_KEYS,
                new RequiredTrait())
        .putMember("status", ExperimentStatusType.$SCHEMA,
                new RequiredTrait())
        .putMember("traffic_percentage", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("context", SharedSchemas.CONDITION,
                new RequiredTrait())
        .putMember("variants", SharedSchemas.LIST_VARIANT,
                new RequiredTrait())
        .putMember("last_modified_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("chosen_variant", PreludeSchemas.STRING)
        .putMember("description", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("started_at", SharedSchemas.DATE_TIME)
        .putMember("started_by", PreludeSchemas.STRING)
        .putMember("metrics_url", PreludeSchemas.STRING)
        .putMember("metrics", PreludeSchemas.DOCUMENT)
        .putMember("experiment_group_id", PreludeSchemas.STRING)
        .build();

    private static final Schema $SCHEMA_ID = $SCHEMA.member("id");
    private static final Schema $SCHEMA_CREATED_AT = $SCHEMA.member("created_at");
    private static final Schema $SCHEMA_CREATED_BY = $SCHEMA.member("created_by");
    private static final Schema $SCHEMA_LAST_MODIFIED = $SCHEMA.member("last_modified");
    private static final Schema $SCHEMA_NAME = $SCHEMA.member("name");
    private static final Schema $SCHEMA_EXPERIMENT_TYPE = $SCHEMA.member("experiment_type");
    private static final Schema $SCHEMA_OVERRIDE_KEYS = $SCHEMA.member("override_keys");
    private static final Schema $SCHEMA_STATUS = $SCHEMA.member("status");
    private static final Schema $SCHEMA_TRAFFIC_PERCENTAGE = $SCHEMA.member("traffic_percentage");
    private static final Schema $SCHEMA_CONTEXT = $SCHEMA.member("context");
    private static final Schema $SCHEMA_VARIANTS = $SCHEMA.member("variants");
    private static final Schema $SCHEMA_LAST_MODIFIED_BY = $SCHEMA.member("last_modified_by");
    private static final Schema $SCHEMA_CHOSEN_VARIANT = $SCHEMA.member("chosen_variant");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");
    private static final Schema $SCHEMA_STARTED_AT = $SCHEMA.member("started_at");
    private static final Schema $SCHEMA_STARTED_BY = $SCHEMA.member("started_by");
    private static final Schema $SCHEMA_METRICS_URL = $SCHEMA.member("metrics_url");
    private static final Schema $SCHEMA_METRICS = $SCHEMA.member("metrics");
    private static final Schema $SCHEMA_EXPERIMENT_GROUP_ID = $SCHEMA.member("experiment_group_id");

    private final transient String id;
    private final transient Instant createdAt;
    private final transient String createdBy;
    private final transient Instant lastModified;
    private final transient String name;
    private final transient ExperimentType experimentType;
    private final transient List<String> overrideKeys;
    private final transient ExperimentStatusType status;
    private final transient int trafficPercentage;
    private final transient Map<String, Document> context;
    private final transient List<Variant> variants;
    private final transient String lastModifiedBy;
    private final transient String chosenVariant;
    private final transient String description;
    private final transient String changeReason;
    private final transient Instant startedAt;
    private final transient String startedBy;
    private final transient String metricsUrl;
    private final transient Document metrics;
    private final transient String experimentGroupId;

    private ConcludeExperimentOutput(Builder builder) {
        this.id = builder.id;
        this.createdAt = builder.createdAt;
        this.createdBy = builder.createdBy;
        this.lastModified = builder.lastModified;
        this.name = builder.name;
        this.experimentType = builder.experimentType;
        this.overrideKeys = Collections.unmodifiableList(builder.overrideKeys);
        this.status = builder.status;
        this.trafficPercentage = builder.trafficPercentage;
        this.context = Collections.unmodifiableMap(builder.context);
        this.variants = Collections.unmodifiableList(builder.variants);
        this.lastModifiedBy = builder.lastModifiedBy;
        this.chosenVariant = builder.chosenVariant;
        this.description = builder.description;
        this.changeReason = builder.changeReason;
        this.startedAt = builder.startedAt;
        this.startedBy = builder.startedBy;
        this.metricsUrl = builder.metricsUrl;
        this.metrics = builder.metrics;
        this.experimentGroupId = builder.experimentGroupId;
    }

    public String id() {
        return id;
    }

    public Instant createdAt() {
        return createdAt;
    }

    public String createdBy() {
        return createdBy;
    }

    public Instant lastModified() {
        return lastModified;
    }

    public String name() {
        return name;
    }

    public ExperimentType experimentType() {
        return experimentType;
    }

    public List<String> overrideKeys() {
        return overrideKeys;
    }

    public boolean hasOverrideKeys() {
        return true;
    }

    public ExperimentStatusType status() {
        return status;
    }

    public int trafficPercentage() {
        return trafficPercentage;
    }

    public Map<String, Document> context() {
        return context;
    }

    public boolean hasContext() {
        return true;
    }

    public List<Variant> variants() {
        return variants;
    }

    public boolean hasVariants() {
        return true;
    }

    public String lastModifiedBy() {
        return lastModifiedBy;
    }

    public String chosenVariant() {
        return chosenVariant;
    }

    public String description() {
        return description;
    }

    public String changeReason() {
        return changeReason;
    }

    public Instant startedAt() {
        return startedAt;
    }

    public String startedBy() {
        return startedBy;
    }

    public String metricsUrl() {
        return metricsUrl;
    }

    public Document metrics() {
        return metrics;
    }

    public String experimentGroupId() {
        return experimentGroupId;
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
        ConcludeExperimentOutput that = (ConcludeExperimentOutput) other;
        return Objects.equals(this.id, that.id)
               && Objects.equals(this.createdAt, that.createdAt)
               && Objects.equals(this.createdBy, that.createdBy)
               && Objects.equals(this.lastModified, that.lastModified)
               && Objects.equals(this.name, that.name)
               && Objects.equals(this.experimentType, that.experimentType)
               && Objects.equals(this.overrideKeys, that.overrideKeys)
               && Objects.equals(this.status, that.status)
               && this.trafficPercentage == that.trafficPercentage
               && Objects.equals(this.context, that.context)
               && Objects.equals(this.variants, that.variants)
               && Objects.equals(this.lastModifiedBy, that.lastModifiedBy)
               && Objects.equals(this.chosenVariant, that.chosenVariant)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.changeReason, that.changeReason)
               && Objects.equals(this.startedAt, that.startedAt)
               && Objects.equals(this.startedBy, that.startedBy)
               && Objects.equals(this.metricsUrl, that.metricsUrl)
               && Objects.equals(this.metrics, that.metrics)
               && Objects.equals(this.experimentGroupId, that.experimentGroupId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, createdAt, createdBy, lastModified, name, experimentType, overrideKeys, status, trafficPercentage, context, variants, lastModifiedBy, chosenVariant, description, changeReason, startedAt, startedBy, metricsUrl, metrics, experimentGroupId);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_ID, id);
        serializer.writeTimestamp($SCHEMA_CREATED_AT, createdAt);
        serializer.writeString($SCHEMA_CREATED_BY, createdBy);
        serializer.writeTimestamp($SCHEMA_LAST_MODIFIED, lastModified);
        serializer.writeString($SCHEMA_NAME, name);
        serializer.writeString($SCHEMA_EXPERIMENT_TYPE, experimentType.value());
        serializer.writeList($SCHEMA_OVERRIDE_KEYS, overrideKeys, overrideKeys.size(), SharedSerde.ListOverrideKeysSerializer.INSTANCE);
        serializer.writeString($SCHEMA_STATUS, status.value());
        serializer.writeInteger($SCHEMA_TRAFFIC_PERCENTAGE, trafficPercentage);
        serializer.writeMap($SCHEMA_CONTEXT, context, context.size(), SharedSerde.ConditionSerializer.INSTANCE);
        serializer.writeList($SCHEMA_VARIANTS, variants, variants.size(), SharedSerde.ListVariantSerializer.INSTANCE);
        serializer.writeString($SCHEMA_LAST_MODIFIED_BY, lastModifiedBy);
        if (chosenVariant != null) {
            serializer.writeString($SCHEMA_CHOSEN_VARIANT, chosenVariant);
        }
        serializer.writeString($SCHEMA_DESCRIPTION, description);
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
        if (startedAt != null) {
            serializer.writeTimestamp($SCHEMA_STARTED_AT, startedAt);
        }
        if (startedBy != null) {
            serializer.writeString($SCHEMA_STARTED_BY, startedBy);
        }
        if (metricsUrl != null) {
            serializer.writeString($SCHEMA_METRICS_URL, metricsUrl);
        }
        if (metrics != null) {
            serializer.writeDocument($SCHEMA_METRICS, metrics);
        }
        if (experimentGroupId != null) {
            serializer.writeString($SCHEMA_EXPERIMENT_GROUP_ID, experimentGroupId);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_ID, member, id);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, createdAt);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, createdBy);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, lastModified);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_NAME, member, name);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_TYPE, member, experimentType);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE_KEYS, member, overrideKeys);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_STATUS, member, status);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_TRAFFIC_PERCENTAGE, member, trafficPercentage);
            case 9 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTEXT, member, context);
            case 10 -> (T) SchemaUtils.validateSameMember($SCHEMA_VARIANTS, member, variants);
            case 11 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, lastModifiedBy);
            case 12 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            case 13 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 14 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHOSEN_VARIANT, member, chosenVariant);
            case 15 -> (T) SchemaUtils.validateSameMember($SCHEMA_STARTED_AT, member, startedAt);
            case 16 -> (T) SchemaUtils.validateSameMember($SCHEMA_STARTED_BY, member, startedBy);
            case 17 -> (T) SchemaUtils.validateSameMember($SCHEMA_METRICS_URL, member, metricsUrl);
            case 18 -> (T) SchemaUtils.validateSameMember($SCHEMA_METRICS, member, metrics);
            case 19 -> (T) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_GROUP_ID, member, experimentGroupId);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ConcludeExperimentOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.id(this.id);
        builder.createdAt(this.createdAt);
        builder.createdBy(this.createdBy);
        builder.lastModified(this.lastModified);
        builder.name(this.name);
        builder.experimentType(this.experimentType);
        builder.overrideKeys(this.overrideKeys);
        builder.status(this.status);
        builder.trafficPercentage(this.trafficPercentage);
        builder.context(this.context);
        builder.variants(this.variants);
        builder.lastModifiedBy(this.lastModifiedBy);
        builder.chosenVariant(this.chosenVariant);
        builder.description(this.description);
        builder.changeReason(this.changeReason);
        builder.startedAt(this.startedAt);
        builder.startedBy(this.startedBy);
        builder.metricsUrl(this.metricsUrl);
        builder.metrics(this.metrics);
        builder.experimentGroupId(this.experimentGroupId);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ConcludeExperimentOutput}.
     */
    public static final class Builder implements ShapeBuilder<ConcludeExperimentOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String id;
        private Instant createdAt;
        private String createdBy;
        private Instant lastModified;
        private String name;
        private ExperimentType experimentType;
        private List<String> overrideKeys;
        private ExperimentStatusType status;
        private int trafficPercentage;
        private Map<String, Document> context;
        private List<Variant> variants;
        private String lastModifiedBy;
        private String chosenVariant;
        private String description;
        private String changeReason;
        private Instant startedAt;
        private String startedBy;
        private String metricsUrl;
        private Document metrics;
        private String experimentGroupId;

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
        public Builder createdAt(Instant createdAt) {
            this.createdAt = Objects.requireNonNull(createdAt, "createdAt cannot be null");
            tracker.setMember($SCHEMA_CREATED_AT);
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
        public Builder lastModified(Instant lastModified) {
            this.lastModified = Objects.requireNonNull(lastModified, "lastModified cannot be null");
            tracker.setMember($SCHEMA_LAST_MODIFIED);
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
        public Builder experimentType(ExperimentType experimentType) {
            this.experimentType = Objects.requireNonNull(experimentType, "experimentType cannot be null");
            tracker.setMember($SCHEMA_EXPERIMENT_TYPE);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder overrideKeys(List<String> overrideKeys) {
            this.overrideKeys = Objects.requireNonNull(overrideKeys, "overrideKeys cannot be null");
            tracker.setMember($SCHEMA_OVERRIDE_KEYS);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder status(ExperimentStatusType status) {
            this.status = Objects.requireNonNull(status, "status cannot be null");
            tracker.setMember($SCHEMA_STATUS);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder trafficPercentage(int trafficPercentage) {
            this.trafficPercentage = trafficPercentage;
            tracker.setMember($SCHEMA_TRAFFIC_PERCENTAGE);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder context(Map<String, Document> context) {
            this.context = Objects.requireNonNull(context, "context cannot be null");
            tracker.setMember($SCHEMA_CONTEXT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder variants(List<Variant> variants) {
            this.variants = Objects.requireNonNull(variants, "variants cannot be null");
            tracker.setMember($SCHEMA_VARIANTS);
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
         * @return this builder.
         */
        public Builder chosenVariant(String chosenVariant) {
            this.chosenVariant = chosenVariant;
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
        public Builder changeReason(String changeReason) {
            this.changeReason = Objects.requireNonNull(changeReason, "changeReason cannot be null");
            tracker.setMember($SCHEMA_CHANGE_REASON);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder startedAt(Instant startedAt) {
            this.startedAt = startedAt;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder startedBy(String startedBy) {
            this.startedBy = startedBy;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder metricsUrl(String metricsUrl) {
            this.metricsUrl = metricsUrl;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder metrics(Document metrics) {
            this.metrics = metrics;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder experimentGroupId(String experimentGroupId) {
            this.experimentGroupId = experimentGroupId;
            return this;
        }

        @Override
        public ConcludeExperimentOutput build() {
            tracker.validate();
            return new ConcludeExperimentOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> id((String) SchemaUtils.validateSameMember($SCHEMA_ID, member, value));
                case 1 -> createdAt((Instant) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, value));
                case 2 -> createdBy((String) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, value));
                case 3 -> lastModified((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, value));
                case 4 -> name((String) SchemaUtils.validateSameMember($SCHEMA_NAME, member, value));
                case 5 -> experimentType((ExperimentType) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_TYPE, member, value));
                case 6 -> overrideKeys((List<String>) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE_KEYS, member, value));
                case 7 -> status((ExperimentStatusType) SchemaUtils.validateSameMember($SCHEMA_STATUS, member, value));
                case 8 -> trafficPercentage((int) SchemaUtils.validateSameMember($SCHEMA_TRAFFIC_PERCENTAGE, member, value));
                case 9 -> context((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_CONTEXT, member, value));
                case 10 -> variants((List<Variant>) SchemaUtils.validateSameMember($SCHEMA_VARIANTS, member, value));
                case 11 -> lastModifiedBy((String) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, value));
                case 12 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 13 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 14 -> chosenVariant((String) SchemaUtils.validateSameMember($SCHEMA_CHOSEN_VARIANT, member, value));
                case 15 -> startedAt((Instant) SchemaUtils.validateSameMember($SCHEMA_STARTED_AT, member, value));
                case 16 -> startedBy((String) SchemaUtils.validateSameMember($SCHEMA_STARTED_BY, member, value));
                case 17 -> metricsUrl((String) SchemaUtils.validateSameMember($SCHEMA_METRICS_URL, member, value));
                case 18 -> metrics((Document) SchemaUtils.validateSameMember($SCHEMA_METRICS, member, value));
                case 19 -> experimentGroupId((String) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_GROUP_ID, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ConcludeExperimentOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_ID)) {
                id("");
            }
            if (!tracker.checkMember($SCHEMA_CREATED_AT)) {
                createdAt(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_CREATED_BY)) {
                createdBy("");
            }
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED)) {
                lastModified(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_NAME)) {
                name("");
            }
            if (!tracker.checkMember($SCHEMA_EXPERIMENT_TYPE)) {
                experimentType(ExperimentType.unknown(""));
            }
            if (!tracker.checkMember($SCHEMA_OVERRIDE_KEYS)) {
                overrideKeys(Collections.emptyList());
            }
            if (!tracker.checkMember($SCHEMA_STATUS)) {
                status(ExperimentStatusType.unknown(""));
            }
            if (!tracker.checkMember($SCHEMA_TRAFFIC_PERCENTAGE)) {
                tracker.setMember($SCHEMA_TRAFFIC_PERCENTAGE);
            }
            if (!tracker.checkMember($SCHEMA_CONTEXT)) {
                context(Collections.emptyMap());
            }
            if (!tracker.checkMember($SCHEMA_VARIANTS)) {
                variants(Collections.emptyList());
            }
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED_BY)) {
                lastModifiedBy("");
            }
            if (!tracker.checkMember($SCHEMA_DESCRIPTION)) {
                description("");
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
                    case 0 -> builder.id(de.readString(member));
                    case 1 -> builder.createdAt(de.readTimestamp(member));
                    case 2 -> builder.createdBy(de.readString(member));
                    case 3 -> builder.lastModified(de.readTimestamp(member));
                    case 4 -> builder.name(de.readString(member));
                    case 5 -> builder.experimentType(ExperimentType.builder().deserializeMember(de, member).build());
                    case 6 -> builder.overrideKeys(SharedSerde.deserializeListOverrideKeys(member, de));
                    case 7 -> builder.status(ExperimentStatusType.builder().deserializeMember(de, member).build());
                    case 8 -> builder.trafficPercentage(de.readInteger(member));
                    case 9 -> builder.context(SharedSerde.deserializeCondition(member, de));
                    case 10 -> builder.variants(SharedSerde.deserializeListVariant(member, de));
                    case 11 -> builder.lastModifiedBy(de.readString(member));
                    case 12 -> builder.description(de.readString(member));
                    case 13 -> builder.changeReason(de.readString(member));
                    case 14 -> builder.chosenVariant(de.readString(member));
                    case 15 -> builder.startedAt(de.readTimestamp(member));
                    case 16 -> builder.startedBy(de.readString(member));
                    case 17 -> builder.metricsUrl(de.readString(member));
                    case 18 -> builder.metrics(de.readDocument());
                    case 19 -> builder.experimentGroupId(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

