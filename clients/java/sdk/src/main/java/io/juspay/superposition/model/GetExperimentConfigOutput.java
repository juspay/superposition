
package io.juspay.superposition.model;

import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
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
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class GetExperimentConfigOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetExperimentConfigOutput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("last_modified", SharedSchemas.DATE_TIME,
                new HttpHeaderTrait("last-modified"),
                new RequiredTrait())
        .putMember("experiments", SharedSchemas.EXPERIMENT_LIST,
                new RequiredTrait())
        .putMember("experiment_groups", SharedSchemas.EXPERIMENT_GROUP_LIST,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_LAST_MODIFIED = $SCHEMA.member("last_modified");
    private static final Schema $SCHEMA_EXPERIMENTS = $SCHEMA.member("experiments");
    private static final Schema $SCHEMA_EXPERIMENT_GROUPS = $SCHEMA.member("experiment_groups");

    private final transient Instant lastModified;
    private final transient List<ExperimentResponse> experiments;
    private final transient List<ExperimentGroupResponse> experimentGroups;

    private GetExperimentConfigOutput(Builder builder) {
        this.lastModified = builder.lastModified;
        this.experiments = Collections.unmodifiableList(builder.experiments);
        this.experimentGroups = Collections.unmodifiableList(builder.experimentGroups);
    }

    public Instant lastModified() {
        return lastModified;
    }

    public List<ExperimentResponse> experiments() {
        return experiments;
    }

    public boolean hasExperiments() {
        return true;
    }

    public List<ExperimentGroupResponse> experimentGroups() {
        return experimentGroups;
    }

    public boolean hasExperimentGroups() {
        return true;
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
        GetExperimentConfigOutput that = (GetExperimentConfigOutput) other;
        return Objects.equals(this.lastModified, that.lastModified)
               && Objects.equals(this.experiments, that.experiments)
               && Objects.equals(this.experimentGroups, that.experimentGroups);
    }

    @Override
    public int hashCode() {
        return Objects.hash(lastModified, experiments, experimentGroups);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeTimestamp($SCHEMA_LAST_MODIFIED, lastModified);
        serializer.writeList($SCHEMA_EXPERIMENTS, experiments, experiments.size(), SharedSerde.ExperimentListSerializer.INSTANCE);
        serializer.writeList($SCHEMA_EXPERIMENT_GROUPS, experimentGroups, experimentGroups.size(), SharedSerde.ExperimentGroupListSerializer.INSTANCE);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, lastModified);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENTS, member, experiments);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_GROUPS, member, experimentGroups);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link GetExperimentConfigOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.lastModified(this.lastModified);
        builder.experiments(this.experiments);
        builder.experimentGroups(this.experimentGroups);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link GetExperimentConfigOutput}.
     */
    public static final class Builder implements ShapeBuilder<GetExperimentConfigOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private Instant lastModified;
        private List<ExperimentResponse> experiments;
        private List<ExperimentGroupResponse> experimentGroups;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
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
        public Builder experiments(List<ExperimentResponse> experiments) {
            this.experiments = Objects.requireNonNull(experiments, "experiments cannot be null");
            tracker.setMember($SCHEMA_EXPERIMENTS);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder experimentGroups(List<ExperimentGroupResponse> experimentGroups) {
            this.experimentGroups = Objects.requireNonNull(experimentGroups, "experimentGroups cannot be null");
            tracker.setMember($SCHEMA_EXPERIMENT_GROUPS);
            return this;
        }

        @Override
        public GetExperimentConfigOutput build() {
            tracker.validate();
            return new GetExperimentConfigOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> lastModified((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, value));
                case 1 -> experiments((List<ExperimentResponse>) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENTS, member, value));
                case 2 -> experimentGroups((List<ExperimentGroupResponse>) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_GROUPS, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<GetExperimentConfigOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED)) {
                lastModified(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_EXPERIMENTS)) {
                experiments(Collections.emptyList());
            }
            if (!tracker.checkMember($SCHEMA_EXPERIMENT_GROUPS)) {
                experimentGroups(Collections.emptyList());
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
                    case 0 -> builder.lastModified(de.readTimestamp(member));
                    case 1 -> builder.experiments(SharedSerde.deserializeExperimentList(member, de));
                    case 2 -> builder.experimentGroups(SharedSerde.deserializeExperimentGroupList(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

