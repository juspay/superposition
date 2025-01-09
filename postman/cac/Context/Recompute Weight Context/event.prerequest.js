const host = pm.variables.get("host");
const org_id = pm.variables.get("org_id");

function update_dimension_position() {
    const options = {
        'method': 'POST',
        'url': `${host}/dimension`,
        'header': {
            'x-tenant': 'test',
            'Content-Type': 'application/json',
            'x-org-id': `${org_id}`
        },
        "body": {
            "mode": "raw",
            "raw": JSON.stringify({
                "dimension": "clientId",
                "position": 1,
                "schema": {
                    "type": "string",
                    "pattern": "^[a-z0-9].*$"
                },
                "description": "description",
                "change_reason": "change_reason"
            })
        }
    };
    pm.sendRequest(options, function (error, response) {
        if (error) {
            console.log(`Error updating dimension: clientId`);
            console.log(error);
            return;
        }
        console.log(`Updated dimension: clientId`);
    });

}

update_dimension_position();
