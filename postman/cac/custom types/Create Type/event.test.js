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
        const modified_response = resp.map(({ created_at, last_modified, ...rest }) => rest);
        pm.expect(modified_response).to.be.eq([
            {
                "created_by": "user@superposition.io",
                "type_name": "Number",
                "type_schema": {
                    "type": "number"
                }
            },
            {
                "created_by": "user@superposition.io",
                "type_name": "Decimal",
                "type_schema": {
                    "type": "number"
                }
            },
            {
                "created_by": "user@superposition.io",
                "type_name": "Boolean",
                "type_schema": {
                    "type": "boolean"
                }
            },
            {
                "created_by": "user@superposition.io",
                "type_name": "String (Enum)",
                "type_schema": {
                    "enum": "$replacement",
                    "type": "string"
                }
            },
            {
                "created_by": "user@superposition.io",
                "type_name": "String (Regex)",
                "type_schema": {
                    "pattern": "$replacement",
                    "type": "string"
                }
            },
            {
                "created_by": "superposition@juspay.in",
                "type_name": "Integer",
                "type_schema": {
                    "type": "number"
                }
            }
        ])
    })
});
