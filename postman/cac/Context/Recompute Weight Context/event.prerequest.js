const host = pm.variables.get("host");

function update_dimension_position() {
    const options = {
        'method': 'PUT',
        'url': `${host}/dimension`,
        'header': {
            'x-tenant': 'test',
            'Content-Type': 'application/json'
        },
        "body": {
            "mode": "raw",
            "raw": JSON.stringify({
                "dimension": "clientId",
                "position": 2,
                "schema": {
                    "type": "string",
                    "pattern": "^[a-z0-9].*$"
                }
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