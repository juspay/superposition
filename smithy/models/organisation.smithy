
$version: "2.0"

namespace io.superposition

use aws.protocols#restJson1

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
    create: CreaterOrganisation
    read: GetOrganisation
    list: ListOrganisation
    put: UpdateOrganisation

}

enum OrgStatus {
    Active = "Active",
    Inactive = "Inactive",
    PendingKyb = "PendingKyb"
}

structure CreateOrganisationRequest for Organisation {
    $country_code

    $contact_email

    $contact_phone

    @required
    $admin_email

    @required
    $name

    $sector

}


structure UpdateOrganisationRequest for Organisation {

    @httpLabel
    @required
    $id

    $country_code

    $contact_email

    $contact_phone

    $admin_email

    $sector

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

structure OrganisationListResponse for Organisation{

    @required
    total_pages: Long

    @required
    total_items: Long

    @required
    data: OrganisationList
    
}


@httpError(404)
@error("client")
structure OrganisationNotFound {}


// Operations
@http(method: "POST", uri: "/superposition/organisations")
operation CreaterOrganisation {
    input : CreateOrganisationRequest
    output: OrganisationResponse
}

@readonly
@http(method: "GET", uri: "/superposition/organisations/{id}")
operation GetOrganisation {
    input := for Organisation {
        @httpLabel
        @required
        $id
    }

    output: OrganisationResponse

    errors: [
        OrganisationNotFound
    ]
}

@idempotent
@http(method: "PUT", uri: "/superposition/organisations/{id}")
operation UpdateOrganisation {
    input : UpdateOrganisationRequest

    output: OrganisationResponse

    errors: [
        OrganisationNotFound
    ]
}

@readonly
@http(method: "GET", uri: "/superposition/organisations")
operation ListOrganisation {
    input :=  for Organisation {
        @httpQuery("page")
        page: Long

        @httpQuery("count")
        count: Long

        @httpQuery("all")
        all: Boolean
    }

    output : OrganisationListResponse
}
