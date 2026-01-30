
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

@SmithyGenerated
public final class RotateWorkspaceEncryptionKeyOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#RotateWorkspaceEncryptionKeyOutput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("total_secrets_re_encrypted", PreludeSchemas.LONG,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_TOTAL_SECRETS_RE_ENCRYPTED = $SCHEMA.member("total_secrets_re_encrypted");

    private final transient long totalSecretsReEncrypted;

    private RotateWorkspaceEncryptionKeyOutput(Builder builder) {
        this.totalSecretsReEncrypted = builder.totalSecretsReEncrypted;
    }

    /**
     * Number of secrets that were re-encrypted with the new key.
     */
    public long totalSecretsReEncrypted() {
        return totalSecretsReEncrypted;
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
        RotateWorkspaceEncryptionKeyOutput that = (RotateWorkspaceEncryptionKeyOutput) other;
        return this.totalSecretsReEncrypted == that.totalSecretsReEncrypted;
    }

    @Override
    public int hashCode() {
        return Objects.hash(totalSecretsReEncrypted);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeLong($SCHEMA_TOTAL_SECRETS_RE_ENCRYPTED, totalSecretsReEncrypted);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_TOTAL_SECRETS_RE_ENCRYPTED, member, totalSecretsReEncrypted);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link RotateWorkspaceEncryptionKeyOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.totalSecretsReEncrypted(this.totalSecretsReEncrypted);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link RotateWorkspaceEncryptionKeyOutput}.
     */
    public static final class Builder implements ShapeBuilder<RotateWorkspaceEncryptionKeyOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private long totalSecretsReEncrypted;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * Number of secrets that were re-encrypted with the new key.
         *
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder totalSecretsReEncrypted(long totalSecretsReEncrypted) {
            this.totalSecretsReEncrypted = totalSecretsReEncrypted;
            tracker.setMember($SCHEMA_TOTAL_SECRETS_RE_ENCRYPTED);
            return this;
        }

        @Override
        public RotateWorkspaceEncryptionKeyOutput build() {
            tracker.validate();
            return new RotateWorkspaceEncryptionKeyOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> totalSecretsReEncrypted((long) SchemaUtils.validateSameMember($SCHEMA_TOTAL_SECRETS_RE_ENCRYPTED, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<RotateWorkspaceEncryptionKeyOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_TOTAL_SECRETS_RE_ENCRYPTED)) {
                tracker.setMember($SCHEMA_TOTAL_SECRETS_RE_ENCRYPTED);
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
                    case 0 -> builder.totalSecretsReEncrypted(de.readLong(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

