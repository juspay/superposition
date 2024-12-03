const host = pm.variables.get("host");

function add_dimension() {
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
                "dimension": "dim1",
                "priority": 4,
                "schema": {
                    "type": "string",
                    "pattern": ".*"
                }
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