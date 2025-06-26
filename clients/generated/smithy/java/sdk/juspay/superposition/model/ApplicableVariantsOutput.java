
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
public final class ApplicableVariantsOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ApplicableVariantsOutput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("data", SharedSchemas.LIST_VARIANT,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_DATA = $SCHEMA.member("data");

    private final transient List<Variant> data;

    private ApplicableVariantsOutput(Builder builder) {
        this.data = Collections.unmodifiableList(builder.data);
    }

    public List<Variant> data() {
        return data;
    }

    public boolean hasData() {
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
        ApplicableVariantsOutput that = (ApplicableVariantsOutput) other;
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
        serializer.writeList($SCHEMA_DATA, data, data.size(), SharedSerde.ListVariantSerializer.INSTANCE);
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
     * @return a builder for {@link ApplicableVariantsOutput}.
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
     * Builder for {@link ApplicableVariantsOutput}.
     */
    public static final class Builder implements ShapeBuilder<ApplicableVariantsOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private List<Variant> data;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder data(List<Variant> data) {
            this.data = Objects.requireNonNull(data, "data cannot be null");
            tracker.setMember($SCHEMA_DATA);
            return this;
        }

        @Override
        public ApplicableVariantsOutput build() {
            tracker.validate();
            return new ApplicableVariantsOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> data((List<Variant>) SchemaUtils.validateSameMember($SCHEMA_DATA, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ApplicableVariantsOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_DATA)) {
                data(Collections.emptyList());
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
                    case 0 -> builder.data(SharedSerde.deserializeListVariant(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

