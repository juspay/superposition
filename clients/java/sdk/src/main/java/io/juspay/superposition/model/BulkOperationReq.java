
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
public final class BulkOperationReq implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#BulkOperationReq");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("operations", SharedSchemas.BULK_OPERATION_LIST)
        .build();

    private static final Schema $SCHEMA_OPERATIONS = $SCHEMA.member("operations");

    private final transient List<ContextAction> operations;

    private BulkOperationReq(Builder builder) {
        this.operations = builder.operations == null ? null : Collections.unmodifiableList(builder.operations);
    }

    public List<ContextAction> operations() {
        if (operations == null) {
            return Collections.emptyList();
        }
        return operations;
    }

    public boolean hasOperations() {
        return operations != null;
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
        BulkOperationReq that = (BulkOperationReq) other;
        return Objects.equals(this.operations, that.operations);
    }

    @Override
    public int hashCode() {
        return Objects.hash(operations);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        if (operations != null) {
            serializer.writeList($SCHEMA_OPERATIONS, operations, operations.size(), SharedSerde.BulkOperationListSerializer.INSTANCE);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_OPERATIONS, member, operations);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link BulkOperationReq}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.operations(this.operations);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link BulkOperationReq}.
     */
    public static final class Builder implements ShapeBuilder<BulkOperationReq> {
        private List<ContextAction> operations;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * @return this builder.
         */
        public Builder operations(List<ContextAction> operations) {
            this.operations = operations;
            return this;
        }

        @Override
        public BulkOperationReq build() {
            return new BulkOperationReq(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> operations((List<ContextAction>) SchemaUtils.validateSameMember($SCHEMA_OPERATIONS, member, value));
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
                    case 0 -> builder.operations(SharedSerde.deserializeBulkOperationList(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

