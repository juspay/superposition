
package io.juspay.superposition.model;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.SchemaUtils;
import software.amazon.smithy.java.core.schema.SerializableStruct;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class WeightRecomputeOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#WeightRecomputeOutput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("data", SharedSchemas.WEIGHT_RECOMPUTE_RESPONSES)
        .build();

    private static final Schema $SCHEMA_DATA = $SCHEMA.member("data");

    private final transient List<WeightRecomputeResponse> data;

    private WeightRecomputeOutput(Builder builder) {
        this.data = builder.data == null ? null : Collections.unmodifiableList(builder.data);
    }

    public List<WeightRecomputeResponse> data() {
        if (data == null) {
            return Collections.emptyList();
        }
        return data;
    }

    public boolean hasData() {
        return data != null;
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
        WeightRecomputeOutput that = (WeightRecomputeOutput) other;
        return Objects.equals(this.data, that.data);
    }

    @Override
    public int hashCode() {
        return Objects.hash(data);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        if (data != null) {
            serializer.writeList($SCHEMA_DATA, data, data.size(), SharedSerde.WeightRecomputeResponsesSerializer.INSTANCE);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_DATA, member, data);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link WeightRecomputeOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.data(this.data);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link WeightRecomputeOutput}.
     */
    public static final class Builder implements ShapeBuilder<WeightRecomputeOutput> {
        private List<WeightRecomputeResponse> data;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * @return this builder.
         */
        public Builder data(List<WeightRecomputeResponse> data) {
            this.data = data;
            return this;
        }

        @Override
        public WeightRecomputeOutput build() {
            return new WeightRecomputeOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> data((List<WeightRecomputeResponse>) SchemaUtils.validateSameMember($SCHEMA_DATA, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
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
                    case 0 -> builder.data(SharedSerde.deserializeWeightRecomputeResponses(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

