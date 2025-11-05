
package io.juspay.superposition.model;

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
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class BulkOperationOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#BulkOperationOutput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("output", SharedSchemas.BULK_OPERATION_OUT_LIST,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_OUTPUT = $SCHEMA.member("output");

    private final transient List<ContextActionOut> output;

    private BulkOperationOutput(Builder builder) {
        this.output = Collections.unmodifiableList(builder.output);
    }

    public List<ContextActionOut> output() {
        return output;
    }

    public boolean hasOutput() {
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
        BulkOperationOutput that = (BulkOperationOutput) other;
        return Objects.equals(this.output, that.output);
    }

    @Override
    public int hashCode() {
        return Objects.hash(output);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeList($SCHEMA_OUTPUT, output, output.size(), SharedSerde.BulkOperationOutListSerializer.INSTANCE);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_OUTPUT, member, output);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link BulkOperationOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.output(this.output);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link BulkOperationOutput}.
     */
    public static final class Builder implements ShapeBuilder<BulkOperationOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private List<ContextActionOut> output;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder output(List<ContextActionOut> output) {
            this.output = Objects.requireNonNull(output, "output cannot be null");
            tracker.setMember($SCHEMA_OUTPUT);
            return this;
        }

        @Override
        public BulkOperationOutput build() {
            tracker.validate();
            return new BulkOperationOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> output((List<ContextActionOut>) SchemaUtils.validateSameMember($SCHEMA_OUTPUT, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<BulkOperationOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_OUTPUT)) {
                output(Collections.emptyList());
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
                    case 0 -> builder.output(SharedSerde.deserializeBulkOperationOutList(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

