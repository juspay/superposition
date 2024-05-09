const host = pm.variables.get("host");

function add_default_config() {
    const options = {
        'method': 'PUT',
        'url': `${host}/default-config/key2`,
        'header': {
            'x-tenant': 'test',
            'Content-Type': 'application/json'
        },
        "body": {
            "mode": "raw",
            "raw": JSON.stringify({
                "value": "value1",
                "schema": {
                    "type": "string",
                    "pattern": ".*"
                }
            })
        }
    };
    pm.sendRequest(options, function (error, response) {
        if (error) {
            console.log(`Error creating default config: new_key`);
            console.log(error);
            return;
        }
        console.log(`created default config: new_key`);
    });
    
}

add_default_config();