
package io.juspay.superposition.model;

import java.util.Objects;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.SchemaUtils;
import software.amazon.smithy.java.core.schema.SerializableStruct;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.HttpPayloadTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class BulkOperationOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#BulkOperationOutput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("bulk_operation_output", BulkOperationOut.$SCHEMA,
                new HttpPayloadTrait())
        .build();

    private static final Schema $SCHEMA_BULK_OPERATION_OUTPUT = $SCHEMA.member("bulk_operation_output");

    private final transient BulkOperationOut bulkOperationOutput;

    private BulkOperationOutput(Builder builder) {
        this.bulkOperationOutput = builder.bulkOperationOutput;
    }

    public BulkOperationOut bulkOperationOutput() {
        return bulkOperationOutput;
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
        return Objects.equals(this.bulkOperationOutput, that.bulkOperationOutput);
    }

    @Override
    public int hashCode() {
        return Objects.hash(bulkOperationOutput);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        if (bulkOperationOutput != null) {
            serializer.writeStruct($SCHEMA_BULK_OPERATION_OUTPUT, bulkOperationOutput);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_BULK_OPERATION_OUTPUT, member, bulkOperationOutput);
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
        builder.bulkOperationOutput(this.bulkOperationOutput);
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
        private BulkOperationOut bulkOperationOutput;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * @return this builder.
         */
        public Builder bulkOperationOutput(BulkOperationOut bulkOperationOutput) {
            this.bulkOperationOutput = bulkOperationOutput;
            return this;
        }

        @Override
        public BulkOperationOutput build() {
            return new BulkOperationOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> bulkOperationOutput((BulkOperationOut) SchemaUtils.validateSameMember($SCHEMA_BULK_OPERATION_OUTPUT, member, value));
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
                    case 0 -> builder.bulkOperationOutput(BulkOperationOut.builder().deserializeMember(de, member).build());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

