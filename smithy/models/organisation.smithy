$version: "2.0"

namespace io.superposition

resource Organisation {
    identifiers: {
        id: String
    }
    properties: {
        name: String
        country_code: String
        contact_email: String
        contact_phone: String
        created_by: String
        admin_email: String
        status: OrgStatus
        sector: String
        created_at: DateTime
        updated_at: DateTime
        updated_by: String
    }
    create: CreateOrganisation
    read: GetOrganisation
    list: ListOrganisation
    update: UpdateOrganisation
}

enum OrgStatus {
    ACTIVE = "Active"
    INACTIVE = "Inactive"
    PENDING_KYB = "PendingKyb"
}

@mixin
structure OrganisationFields for Organisation {
    $country_code
    $contact_email
    $contact_phone
    $admin_email
    $sector
}

structure CreateOrganisationRequest for Organisation with [OrganisationFields] {
    @required
    $admin_email

    @required
    $name
}

structure UpdateOrganisationRequest for Organisation with [OrganisationFields] {
    @httpLabel
    @required
    $id

    $status
}

structure OrganisationResponse for Organisation {
    @required
    $id

    @required
    $name

    $country_code

    $contact_email

    $contact_phone

    @required
    $created_by

    @required
    $admin_email

    @required
    $status

    $sector

    @required
    $created_at

    @required
    $updated_at

    @required
    $updated_by
}

list OrganisationList {
    member: OrganisationResponse
}

// Operations
@documentation("Creates a new organisation with specified name and administrator email. This is the top-level entity that contains workspaces and manages organizational-level settings.")
@http(method: "POST", uri: "/superposition/organisations")
@tags(["Organisation Management"])
operation CreateOrganisation {
    input: CreateOrganisationRequest
    output: OrganisationResponse
}

@documentation("Retrieves detailed information about a specific organisation including its status, contact details, and administrative metadata.")
@readonly
@http(method: "GET", uri: "/superposition/organisations/{id}")
@tags(["Organisation Management"])
operation GetOrganisation with [GetOperation] {
    input := for Organisation {
        @httpLabel
        @required
        $id
    }

    output: OrganisationResponse
}

@documentation("Updates an existing organisation's information including contact details, status, and administrative properties.")
@idempotent
@http(method: "PATCH", uri: "/superposition/organisations/{id}")
@tags(["Organisation Management"])
operation UpdateOrganisation with [GetOperation] {
    input: UpdateOrganisationRequest
    output: OrganisationResponse
}

@documentation("Retrieves a paginated list of all organisations with their basic information, creation details, and current status.")
@readonly
@http(method: "GET", uri: "/superposition/organisations")
@tags(["Organisation Management"])
operation ListOrganisation {
    input := with [PaginationParams] {}
    output := with [PaginatedResponse] {
        data: OrganisationList
    }
}
