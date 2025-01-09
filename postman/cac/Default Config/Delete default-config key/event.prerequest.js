const host = pm.variables.get("host");
const org_id = pm.variables.get("org_id");

function add_default_config() {
    const options = {
        'method': 'POST',
        'url': `${host}/default-config`,
        'header': {
            'x-tenant': 'test',
            'Content-Type': 'application/json',
            'x-org-id': `${org_id}`
        },
        "body": {
            "mode": "raw",
            "raw": JSON.stringify({
                "key": "key2",
                "value": "value1",
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
            console.log(`Error creating default config: new_key`);
            console.log(error);
            return;
        }
        console.log(`created default config: new_key`);
    });

}

add_default_config();
