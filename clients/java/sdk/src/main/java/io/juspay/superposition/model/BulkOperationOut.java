
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
public final class BulkOperationOut implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#BulkOperationOut");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("output", SharedSchemas.BULK_OPERATION_OUT_LIST)
        .build();

    private static final Schema $SCHEMA_OUTPUT = $SCHEMA.member("output");

    private final transient List<ContextActionOut> output;

    private BulkOperationOut(Builder builder) {
        this.output = builder.output == null ? null : Collections.unmodifiableList(builder.output);
    }

    public List<ContextActionOut> output() {
        if (output == null) {
            return Collections.emptyList();
        }
        return output;
    }

    public boolean hasOutput() {
        return output != null;
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
        BulkOperationOut that = (BulkOperationOut) other;
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
        if (output != null) {
            serializer.writeList($SCHEMA_OUTPUT, output, output.size(), SharedSerde.BulkOperationOutListSerializer.INSTANCE);
        }
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
     * @return a builder for {@link BulkOperationOut}.
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
     * Builder for {@link BulkOperationOut}.
     */
    public static final class Builder implements ShapeBuilder<BulkOperationOut> {
        private List<ContextActionOut> output;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * @return this builder.
         */
        public Builder output(List<ContextActionOut> output) {
            this.output = output;
            return this;
        }

        @Override
        public BulkOperationOut build() {
            return new BulkOperationOut(this);
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

