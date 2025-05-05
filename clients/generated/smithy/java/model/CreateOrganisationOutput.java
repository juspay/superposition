
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

@SmithyGenerated
public final class CreateOrganisationOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#OrganisationResponse");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("name", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("country_code", PreludeSchemas.STRING)
        .putMember("contact_email", PreludeSchemas.STRING)
        .putMember("contact_phone", PreludeSchemas.STRING)
        .putMember("created_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("admin_email", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("status", OrgStatus.$SCHEMA,
                new RequiredTrait())
        .putMember("sector", PreludeSchemas.STRING)
        .putMember("created_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("updated_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("updated_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_ID = $SCHEMA.member("id");
    private static final Schema $SCHEMA_NAME = $SCHEMA.member("name");
    private static final Schema $SCHEMA_COUNTRY_CODE = $SCHEMA.member("country_code");
    private static final Schema $SCHEMA_CONTACT_EMAIL = $SCHEMA.member("contact_email");
    private static final Schema $SCHEMA_CONTACT_PHONE = $SCHEMA.member("contact_phone");
    private static final Schema $SCHEMA_CREATED_BY = $SCHEMA.member("created_by");
    private static final Schema $SCHEMA_ADMIN_EMAIL = $SCHEMA.member("admin_email");
    private static final Schema $SCHEMA_STATUS = $SCHEMA.member("status");
    private static final Schema $SCHEMA_SECTOR = $SCHEMA.member("sector");
    private static final Schema $SCHEMA_CREATED_AT = $SCHEMA.member("created_at");
    private static final Schema $SCHEMA_UPDATED_AT = $SCHEMA.member("updated_at");
    private static final Schema $SCHEMA_UPDATED_BY = $SCHEMA.member("updated_by");

    private final transient String id;
    private final transient String name;
    private final transient String countryCode;
    private final transient String contactEmail;
    private final transient String contactPhone;
    private final transient String createdBy;
    private final transient String adminEmail;
    private final transient OrgStatus status;
    private final transient String sector;
    private final transient Instant createdAt;
    private final transient Instant updatedAt;
    private final transient String updatedBy;

    private CreateOrganisationOutput(Builder builder) {
        this.id = builder.id;
        this.name = builder.name;
        this.countryCode = builder.countryCode;
        this.contactEmail = builder.contactEmail;
        this.contactPhone = builder.contactPhone;
        this.createdBy = builder.createdBy;
        this.adminEmail = builder.adminEmail;
        this.status = builder.status;
        this.sector = builder.sector;
        this.createdAt = builder.createdAt;
        this.updatedAt = builder.updatedAt;
        this.updatedBy = builder.updatedBy;
    }

    public String id() {
        return id;
    }

    public String name() {
        return name;
    }

    public String countryCode() {
        return countryCode;
    }

    public String contactEmail() {
        return contactEmail;
    }

    public String contactPhone() {
        return contactPhone;
    }

    public String createdBy() {
        return createdBy;
    }

    public String adminEmail() {
        return adminEmail;
    }

    public OrgStatus status() {
        return status;
    }

    public String sector() {
        return sector;
    }

    public Instant createdAt() {
        return createdAt;
    }

    public Instant updatedAt() {
        return updatedAt;
    }

    public String updatedBy() {
        return updatedBy;
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
        CreateOrganisationOutput that = (CreateOrganisationOutput) other;
        return Objects.equals(this.id, that.id)
               && Objects.equals(this.name, that.name)
               && Objects.equals(this.countryCode, that.countryCode)
               && Objects.equals(this.contactEmail, that.contactEmail)
               && Objects.equals(this.contactPhone, that.contactPhone)
               && Objects.equals(this.createdBy, that.createdBy)
               && Objects.equals(this.adminEmail, that.adminEmail)
               && Objects.equals(this.status, that.status)
               && Objects.equals(this.sector, that.sector)
               && Objects.equals(this.createdAt, that.createdAt)
               && Objects.equals(this.updatedAt, that.updatedAt)
               && Objects.equals(this.updatedBy, that.updatedBy);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, name, countryCode, contactEmail, contactPhone, createdBy, adminEmail, status, sector, createdAt, updatedAt, updatedBy);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_ID, id);
        serializer.writeString($SCHEMA_NAME, name);
        if (countryCode != null) {
            serializer.writeString($SCHEMA_COUNTRY_CODE, countryCode);
        }
        if (contactEmail != null) {
            serializer.writeString($SCHEMA_CONTACT_EMAIL, contactEmail);
        }
        if (contactPhone != null) {
            serializer.writeString($SCHEMA_CONTACT_PHONE, contactPhone);
        }
        serializer.writeString($SCHEMA_CREATED_BY, createdBy);
        serializer.writeString($SCHEMA_ADMIN_EMAIL, adminEmail);
        serializer.writeString($SCHEMA_STATUS, status.value());
        if (sector != null) {
            serializer.writeString($SCHEMA_SECTOR, sector);
        }
        serializer.writeTimestamp($SCHEMA_CREATED_AT, createdAt);
        serializer.writeTimestamp($SCHEMA_UPDATED_AT, updatedAt);
        serializer.writeString($SCHEMA_UPDATED_BY, updatedBy);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_ID, member, id);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_NAME, member, name);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, createdBy);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_ADMIN_EMAIL, member, adminEmail);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_STATUS, member, status);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, createdAt);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_UPDATED_AT, member, updatedAt);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_UPDATED_BY, member, updatedBy);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_COUNTRY_CODE, member, countryCode);
            case 9 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTACT_EMAIL, member, contactEmail);
            case 10 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTACT_PHONE, member, contactPhone);
            case 11 -> (T) SchemaUtils.validateSameMember($SCHEMA_SECTOR, member, sector);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link CreateOrganisationOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.id(this.id);
        builder.name(this.name);
        builder.countryCode(this.countryCode);
        builder.contactEmail(this.contactEmail);
        builder.contactPhone(this.contactPhone);
        builder.createdBy(this.createdBy);
        builder.adminEmail(this.adminEmail);
        builder.status(this.status);
        builder.sector(this.sector);
        builder.createdAt(this.createdAt);
        builder.updatedAt(this.updatedAt);
        builder.updatedBy(this.updatedBy);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link CreateOrganisationOutput}.
     */
    public static final class Builder implements ShapeBuilder<CreateOrganisationOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String id;
        private String name;
        private String countryCode;
        private String contactEmail;
        private String contactPhone;
        private String createdBy;
        private String adminEmail;
        private OrgStatus status;
        private String sector;
        private Instant createdAt;
        private Instant updatedAt;
        private String updatedBy;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder id(String id) {
            this.id = Objects.requireNonNull(id, "id cannot be null");
            tracker.setMember($SCHEMA_ID);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder name(String name) {
            this.name = Objects.requireNonNull(name, "name cannot be null");
            tracker.setMember($SCHEMA_NAME);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder countryCode(String countryCode) {
            this.countryCode = countryCode;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder contactEmail(String contactEmail) {
            this.contactEmail = contactEmail;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder contactPhone(String contactPhone) {
            this.contactPhone = contactPhone;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder createdBy(String createdBy) {
            this.createdBy = Objects.requireNonNull(createdBy, "createdBy cannot be null");
            tracker.setMember($SCHEMA_CREATED_BY);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder adminEmail(String adminEmail) {
            this.adminEmail = Objects.requireNonNull(adminEmail, "adminEmail cannot be null");
            tracker.setMember($SCHEMA_ADMIN_EMAIL);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder status(OrgStatus status) {
            this.status = Objects.requireNonNull(status, "status cannot be null");
            tracker.setMember($SCHEMA_STATUS);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder sector(String sector) {
            this.sector = sector;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder createdAt(Instant createdAt) {
            this.createdAt = Objects.requireNonNull(createdAt, "createdAt cannot be null");
            tracker.setMember($SCHEMA_CREATED_AT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder updatedAt(Instant updatedAt) {
            this.updatedAt = Objects.requireNonNull(updatedAt, "updatedAt cannot be null");
            tracker.setMember($SCHEMA_UPDATED_AT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder updatedBy(String updatedBy) {
            this.updatedBy = Objects.requireNonNull(updatedBy, "updatedBy cannot be null");
            tracker.setMember($SCHEMA_UPDATED_BY);
            return this;
        }

        @Override
        public CreateOrganisationOutput build() {
            tracker.validate();
            return new CreateOrganisationOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> id((String) SchemaUtils.validateSameMember($SCHEMA_ID, member, value));
                case 1 -> name((String) SchemaUtils.validateSameMember($SCHEMA_NAME, member, value));
                case 2 -> createdBy((String) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, value));
                case 3 -> adminEmail((String) SchemaUtils.validateSameMember($SCHEMA_ADMIN_EMAIL, member, value));
                case 4 -> status((OrgStatus) SchemaUtils.validateSameMember($SCHEMA_STATUS, member, value));
                case 5 -> createdAt((Instant) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, value));
                case 6 -> updatedAt((Instant) SchemaUtils.validateSameMember($SCHEMA_UPDATED_AT, member, value));
                case 7 -> updatedBy((String) SchemaUtils.validateSameMember($SCHEMA_UPDATED_BY, member, value));
                case 8 -> countryCode((String) SchemaUtils.validateSameMember($SCHEMA_COUNTRY_CODE, member, value));
                case 9 -> contactEmail((String) SchemaUtils.validateSameMember($SCHEMA_CONTACT_EMAIL, member, value));
                case 10 -> contactPhone((String) SchemaUtils.validateSameMember($SCHEMA_CONTACT_PHONE, member, value));
                case 11 -> sector((String) SchemaUtils.validateSameMember($SCHEMA_SECTOR, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<CreateOrganisationOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_ID)) {
                id("");
            }
            if (!tracker.checkMember($SCHEMA_NAME)) {
                name("");
            }
            if (!tracker.checkMember($SCHEMA_CREATED_BY)) {
                createdBy("");
            }
            if (!tracker.checkMember($SCHEMA_ADMIN_EMAIL)) {
                adminEmail("");
            }
            if (!tracker.checkMember($SCHEMA_STATUS)) {
                status(OrgStatus.unknown(""));
            }
            if (!tracker.checkMember($SCHEMA_CREATED_AT)) {
                createdAt(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_UPDATED_AT)) {
                updatedAt(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_UPDATED_BY)) {
                updatedBy("");
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
                    case 0 -> builder.id(de.readString(member));
                    case 1 -> builder.name(de.readString(member));
                    case 2 -> builder.createdBy(de.readString(member));
                    case 3 -> builder.adminEmail(de.readString(member));
                    case 4 -> builder.status(OrgStatus.builder().deserializeMember(de, member).build());
                    case 5 -> builder.createdAt(de.readTimestamp(member));
                    case 6 -> builder.updatedAt(de.readTimestamp(member));
                    case 7 -> builder.updatedBy(de.readString(member));
                    case 8 -> builder.countryCode(de.readString(member));
                    case 9 -> builder.contactEmail(de.readString(member));
                    case 10 -> builder.contactPhone(de.readString(member));
                    case 11 -> builder.sector(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

