const host = pm.variables.get("host");

function add_dimension() {
    const options = {
        'method': 'POST',
        'url': `${host}/dimension`,
        'header': {
            'x-tenant': 'test',
            'Content-Type': 'application/json'
        },
        "body": {
            "mode": "raw",
            "raw": JSON.stringify({
                "dimension": "variantIds",
                "position": 0,
                "schema": {
                    "type": "string",
                    "pattern": "^[a-z0-9].*$"
                }, 
                "description": "change_reason",
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