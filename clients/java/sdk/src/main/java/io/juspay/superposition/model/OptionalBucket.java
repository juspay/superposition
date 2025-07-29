
package io.juspay.superposition.model;

import java.util.Objects;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.SchemaUtils;
import software.amazon.smithy.java.core.schema.SerializableStruct;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.schema.Unit;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public abstract class OptionalBucket implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#OptionalBucket");

    public static final Schema $SCHEMA = Schema.unionBuilder($ID)
        .putMember("bucket", Bucket.$SCHEMA)
        .putMember("null", Unit.SCHEMA)
        .build();

    private static final Schema $SCHEMA_BUCKET = $SCHEMA.member("bucket");
    private static final Schema $SCHEMA_NULL = $SCHEMA.member("null");

    private final Type type;

    private OptionalBucket(Type type) {
        this.type = type;
    }

    public Type type() {
        return type;
    }

    /**
     * Enum representing the possible variants of {@link OptionalBucket}.
     */
    public enum Type {
        $UNKNOWN,
        bucket,
        null
    }

    @Override
    public String toString() {
        return ToStringSerializer.serialize(this);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public <T> T getMemberValue(Schema member) {
        return SchemaUtils.validateMemberInSchema($SCHEMA, member, getValue());
    }

    public abstract <T> T getValue();

    @SmithyGenerated
    public static final class BucketMember extends OptionalBucket {
        private final transient Bucket value;

        public BucketMember(Bucket value) {
            super(Type.bucket);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeStruct($SCHEMA_BUCKET, value);
        }

        public Bucket bucket() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) value;
        }
    }

    @SmithyGenerated
    public static final class NullMember extends OptionalBucket {

        public NullMember() {
            super(Type.null);
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeStruct($SCHEMA_NULL, Unit.getInstance());
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) null;
        }
    }

    public static final class $UnknownMember extends OptionalBucket {
        private final String memberName;

        public $UnknownMember(String memberName) {
            super(Type.$UNKNOWN);
            this.memberName = memberName;
        }

        public String memberName() {
            return memberName;
        }

        @Override
        public void serialize(ShapeSerializer serializer) {
            throw new UnsupportedOperationException("Cannot serialize union with unknown member " + this.memberName);
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {}

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) memberName;
        }
    }

    @Override
    public int hashCode() {
        return Objects.hash(type, getValue());
    }

    @Override
    public boolean equals(Object other) {
        if (other == this) {
            return true;
        }
        if (other == null || getClass() != other.getClass()) {
            return false;
        }
        return Objects.equals(getValue(), ((OptionalBucket) other).getValue());
    }

    public interface BuildStage {
        OptionalBucket build();
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link OptionalBucket}.
     */
    public static final class Builder implements ShapeBuilder<OptionalBucket>, BuildStage {
        private OptionalBucket value;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        public BuildStage bucket(Bucket value) {
            return setValue(new BucketMember(value));
        }

        public BuildStage null(Unit value) {
            return setValue(new NullMember());
        }

        public BuildStage $unknownMember(String memberName) {
            return setValue(new $UnknownMember(memberName));
        }

        private BuildStage setValue(OptionalBucket value) {
            if (this.value != null) {
                if (this.value.type() == Type.$UNKNOWN) {
                    throw new IllegalArgumentException("Cannot change union from unknown to known variant");
                }
                throw new IllegalArgumentException("Only one value may be set for unions");
            }
            this.value = value;
            return this;
        }

        @Override
        public OptionalBucket build() {
            return Objects.requireNonNull(value, "no union value set");
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> bucket((Bucket) SchemaUtils.validateSameMember($SCHEMA_BUCKET, member, value));
                case 1 -> null((Unit) SchemaUtils.validateSameMember($SCHEMA_NULL, member, value));
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
                    case 0 -> builder.bucket(Bucket.builder().deserializeMember(de, member).build());
                    case 1 -> builder.null(Unit.builder().deserializeMember(de, member).build());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }

            @Override
            public void unknownMember(Builder builder, String memberName) {
                builder.$unknownMember(memberName);
            }
        }
    }
}

