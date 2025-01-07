const host = pm.environment.get("host");
const token = pm.environment.get("token");
const org_id = pm.environment.get("org_id");

function create_default_config_keys() {
    let keys = [
        `pmTestKey4`,
        `pmTestKey3`
    ];

    for (const key of keys) {
        const options = {
            'method': 'POST',
            'url': `${host}/default-config`,
            "header": {
                'Authorization': `Bearer ${token}`,
                'x-tenant': 'test',
                'Content-Type': 'application/json',
                'x-org-id': `${org_id}`
            },
            "body": {
                "mode": "raw",
                "raw": JSON.stringify({ 
                    "key": key,
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
        console.log(options);
        pm.sendRequest(options, function (error, response) {
            if (error) {
                console.log(`Error creating default-config key: ${key}`);
                console.log(error);
                return;
            };
            console.log(`Created default-config key: ${key}`);
        });
    }
}

create_default_config_keys();