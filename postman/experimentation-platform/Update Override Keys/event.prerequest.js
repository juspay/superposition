const host = pm.environment.get("host");
const token = pm.environment.get("token");

function create_default_config_keys() {
    let keys = [
        `pmTestKey1972`,
        `pmTestKey1999`
    ];

    for (const key of keys) {
        const options = {
            'method': 'PUT',
            'url': `${host}/default-config/${key}`,
            "header": {
                'Authorization': `Bearer ${token}`,
                'x-tenant': 'mjos',
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

create_default_config_keys()