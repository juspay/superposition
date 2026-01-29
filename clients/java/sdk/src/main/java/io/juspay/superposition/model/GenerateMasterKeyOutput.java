
package io.juspay.superposition.model;

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
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

/**
 * Response containing newly generated master key
 */
@SmithyGenerated
public final class GenerateMasterKeyOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GenerateMasterKeyResponse");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("master_key", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("instructions", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("warning", PreludeSchemas.STRING,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_MASTER_KEY = $SCHEMA.member("master_key");
    private static final Schema $SCHEMA_INSTRUCTIONS = $SCHEMA.member("instructions");
    private static final Schema $SCHEMA_WARNING = $SCHEMA.member("warning");

    private final transient String masterKey;
    private final transient String instructions;
    private final transient String warning;

    private GenerateMasterKeyOutput(Builder builder) {
        this.masterKey = builder.masterKey;
        this.instructions = builder.instructions;
        this.warning = builder.warning;
    }

    public String masterKey() {
        return masterKey;
    }

    public String instructions() {
        return instructions;
    }

    public String warning() {
        return warning;
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
        GenerateMasterKeyOutput that = (GenerateMasterKeyOutput) other;
        return Objects.equals(this.masterKey, that.masterKey)
               && Objects.equals(this.instructions, that.instructions)
               && Objects.equals(this.warning, that.warning);
    }

    @Override
    public int hashCode() {
        return Objects.hash(masterKey, instructions, warning);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_MASTER_KEY, masterKey);
        serializer.writeString($SCHEMA_INSTRUCTIONS, instructions);
        serializer.writeString($SCHEMA_WARNING, warning);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_MASTER_KEY, member, masterKey);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_INSTRUCTIONS, member, instructions);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_WARNING, member, warning);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link GenerateMasterKeyOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.masterKey(this.masterKey);
        builder.instructions(this.instructions);
        builder.warning(this.warning);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link GenerateMasterKeyOutput}.
     */
    public static final class Builder implements ShapeBuilder<GenerateMasterKeyOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String masterKey;
        private String instructions;
        private String warning;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder masterKey(String masterKey) {
            this.masterKey = Objects.requireNonNull(masterKey, "masterKey cannot be null");
            tracker.setMember($SCHEMA_MASTER_KEY);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder instructions(String instructions) {
            this.instructions = Objects.requireNonNull(instructions, "instructions cannot be null");
            tracker.setMember($SCHEMA_INSTRUCTIONS);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder warning(String warning) {
            this.warning = Objects.requireNonNull(warning, "warning cannot be null");
            tracker.setMember($SCHEMA_WARNING);
            return this;
        }

        @Override
        public GenerateMasterKeyOutput build() {
            tracker.validate();
            return new GenerateMasterKeyOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> masterKey((String) SchemaUtils.validateSameMember($SCHEMA_MASTER_KEY, member, value));
                case 1 -> instructions((String) SchemaUtils.validateSameMember($SCHEMA_INSTRUCTIONS, member, value));
                case 2 -> warning((String) SchemaUtils.validateSameMember($SCHEMA_WARNING, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<GenerateMasterKeyOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_MASTER_KEY)) {
                masterKey("");
            }
            if (!tracker.checkMember($SCHEMA_INSTRUCTIONS)) {
                instructions("");
            }
            if (!tracker.checkMember($SCHEMA_WARNING)) {
                warning("");
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
                    case 0 -> builder.masterKey(de.readString(member));
                    case 1 -> builder.instructions(de.readString(member));
                    case 2 -> builder.warning(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

