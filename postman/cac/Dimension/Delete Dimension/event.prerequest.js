const host = pm.variables.get("host");
const org_id = pm.variables.get("org_id");

function add_dimension() {
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
                "dimension": "dim1",
                "position": 2,
                "schema": {
                    "type": "string",
                    "pattern": ".*"
                }, 
                "description": "description",
                "change_reason": "change_reason"
            })
        }
    };
    pm.sendRequest(options, function (error, response) {
        if (error) {
            console.log(`Error creating dimension: dim1`);
            console.log(error);
            return;
        }
        console.log(`created dimension: dim1`);
    });
    
}

add_dimension();