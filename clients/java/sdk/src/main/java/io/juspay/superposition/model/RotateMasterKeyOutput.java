
package io.juspay.superposition.model;

import java.time.Instant;
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
 * Response after generating the master key
 */
@SmithyGenerated
public final class RotateMasterKeyOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#RotateMasterKeyResponse");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspaces_rotated", PreludeSchemas.LONG,
                new RequiredTrait())
        .putMember("total_secrets_re_encrypted", PreludeSchemas.LONG,
                new RequiredTrait())
        .putMember("rotated_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("new_master_key", PreludeSchemas.STRING,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_WORKSPACES_ROTATED = $SCHEMA.member("workspaces_rotated");
    private static final Schema $SCHEMA_TOTAL_SECRETS_RE_ENCRYPTED = $SCHEMA.member("total_secrets_re_encrypted");
    private static final Schema $SCHEMA_ROTATED_AT = $SCHEMA.member("rotated_at");
    private static final Schema $SCHEMA_NEW_MASTER_KEY = $SCHEMA.member("new_master_key");

    private final transient long workspacesRotated;
    private final transient long totalSecretsReEncrypted;
    private final transient Instant rotatedAt;
    private final transient String newMasterKey;

    private RotateMasterKeyOutput(Builder builder) {
        this.workspacesRotated = builder.workspacesRotated;
        this.totalSecretsReEncrypted = builder.totalSecretsReEncrypted;
        this.rotatedAt = builder.rotatedAt;
        this.newMasterKey = builder.newMasterKey;
    }

    public long workspacesRotated() {
        return workspacesRotated;
    }

    public long totalSecretsReEncrypted() {
        return totalSecretsReEncrypted;
    }

    public Instant rotatedAt() {
        return rotatedAt;
    }

    public String newMasterKey() {
        return newMasterKey;
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
        RotateMasterKeyOutput that = (RotateMasterKeyOutput) other;
        return this.workspacesRotated == that.workspacesRotated
               && this.totalSecretsReEncrypted == that.totalSecretsReEncrypted
               && Objects.equals(this.rotatedAt, that.rotatedAt)
               && Objects.equals(this.newMasterKey, that.newMasterKey);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspacesRotated, totalSecretsReEncrypted, rotatedAt, newMasterKey);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeLong($SCHEMA_WORKSPACES_ROTATED, workspacesRotated);
        serializer.writeLong($SCHEMA_TOTAL_SECRETS_RE_ENCRYPTED, totalSecretsReEncrypted);
        serializer.writeTimestamp($SCHEMA_ROTATED_AT, rotatedAt);
        serializer.writeString($SCHEMA_NEW_MASTER_KEY, newMasterKey);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACES_ROTATED, member, workspacesRotated);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_TOTAL_SECRETS_RE_ENCRYPTED, member, totalSecretsReEncrypted);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_ROTATED_AT, member, rotatedAt);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_NEW_MASTER_KEY, member, newMasterKey);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link RotateMasterKeyOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspacesRotated(this.workspacesRotated);
        builder.totalSecretsReEncrypted(this.totalSecretsReEncrypted);
        builder.rotatedAt(this.rotatedAt);
        builder.newMasterKey(this.newMasterKey);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link RotateMasterKeyOutput}.
     */
    public static final class Builder implements ShapeBuilder<RotateMasterKeyOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private long workspacesRotated;
        private long totalSecretsReEncrypted;
        private Instant rotatedAt;
        private String newMasterKey;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder workspacesRotated(long workspacesRotated) {
            this.workspacesRotated = workspacesRotated;
            tracker.setMember($SCHEMA_WORKSPACES_ROTATED);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder totalSecretsReEncrypted(long totalSecretsReEncrypted) {
            this.totalSecretsReEncrypted = totalSecretsReEncrypted;
            tracker.setMember($SCHEMA_TOTAL_SECRETS_RE_ENCRYPTED);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder rotatedAt(Instant rotatedAt) {
            this.rotatedAt = Objects.requireNonNull(rotatedAt, "rotatedAt cannot be null");
            tracker.setMember($SCHEMA_ROTATED_AT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder newMasterKey(String newMasterKey) {
            this.newMasterKey = Objects.requireNonNull(newMasterKey, "newMasterKey cannot be null");
            tracker.setMember($SCHEMA_NEW_MASTER_KEY);
            return this;
        }

        @Override
        public RotateMasterKeyOutput build() {
            tracker.validate();
            return new RotateMasterKeyOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspacesRotated((long) SchemaUtils.validateSameMember($SCHEMA_WORKSPACES_ROTATED, member, value));
                case 1 -> totalSecretsReEncrypted((long) SchemaUtils.validateSameMember($SCHEMA_TOTAL_SECRETS_RE_ENCRYPTED, member, value));
                case 2 -> rotatedAt((Instant) SchemaUtils.validateSameMember($SCHEMA_ROTATED_AT, member, value));
                case 3 -> newMasterKey((String) SchemaUtils.validateSameMember($SCHEMA_NEW_MASTER_KEY, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<RotateMasterKeyOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACES_ROTATED)) {
                tracker.setMember($SCHEMA_WORKSPACES_ROTATED);
            }
            if (!tracker.checkMember($SCHEMA_TOTAL_SECRETS_RE_ENCRYPTED)) {
                tracker.setMember($SCHEMA_TOTAL_SECRETS_RE_ENCRYPTED);
            }
            if (!tracker.checkMember($SCHEMA_ROTATED_AT)) {
                rotatedAt(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_NEW_MASTER_KEY)) {
                newMasterKey("");
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
                    case 0 -> builder.workspacesRotated(de.readLong(member));
                    case 1 -> builder.totalSecretsReEncrypted(de.readLong(member));
                    case 2 -> builder.rotatedAt(de.readTimestamp(member));
                    case 3 -> builder.newMasterKey(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

