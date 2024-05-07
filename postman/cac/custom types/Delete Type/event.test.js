/* global pm */

pm.test('expect response be 200', function () {
    pm.response.to.be.ok;
    const host = pm.variables.get("host");
    const request = {
        url: `${host}/types`,
        method: 'GET',
        header: {
            'Content-Type': 'application/json',
            'x-tenant': 'test',
        }
    }
    pm.sendRequest(request, (error, response) => {
        if(error) {
            console.log("Failed to fetch types");
            throw error;
        }
        const resp = response.json();
        const modified_response = resp.map(({ created_at, id, last_modified, ...rest }) => rest);
        pm.expect(modified_response).to.be.eq([
            {
                "created_by": "user@superposition.io",
                "display_name": "Number",
                "type_name": "number",
                "type_schema": {
                    "type": "number"
                }
            },
            {
                "created_by": "user@superposition.io",
                "display_name": "Decimal",
                "type_name": "number",
                "type_schema": {
                    "type": "number"
                }
            },
            {
                "created_by": "user@superposition.io",
                "display_name": "Boolean",
                "type_name": "boolean",
                "type_schema": {
                    "type": "boolean"
                }
            },
            {
                "created_by": "user@superposition.io",
                "display_name": "String (Enum)",
                "type_name": "enum",
                "type_schema": {
                    "enum": "$replacement",
                    "type": "string"
                }
            },
            {
                "created_by": "user@superposition.io",
                "display_name": "String (Regex)",
                "type_name": "pattern",
                "type_schema": {
                    "pattern": "$replacement",
                    "type": "string"
                }
            },
        ])
    })
});
