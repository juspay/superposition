const host = pm.environment.get("host");
const token = pm.environment.get("token");

function create_default_config_keys() {
    let keys = [
        `pmTestKey1`,
        `pmTestKey2`
    ];

    for (const key of keys) {
        const options = {
            'method': 'PUT',
            'url': `${host}/default-config/${key}`,
            "header": {
                'Authorization': `Bearer ${token}`,
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

function create_dimensions() {
    const dimensions = [
        {name: "os", position: 0, schema: { type: "string", enum: ["android", "ios", "web"] }},
        {name: "client", position: 1, schema: { type: "string", pattern: ".*" }},
        {name: "variantIds", position: 2, schema: { type: "string", pattern: ".*" }}
    ];

    for (const dimension of dimensions) {
        const options = {
            'method': 'PUT',
            'url': `${host}/dimension`,
            'header': {
                'Authorization': `Bearer ${token}`,
                'x-tenant': 'test',
                'Content-Type': 'application/json'
            },
            "body": {
                "mode": "raw",
                "raw": JSON.stringify({
                    "dimension": dimension.name,
                    "position": dimension.position,
                    "schema": dimension.schema
                })
            }
        };
        pm.sendRequest(options, function (error, response) {
            if (error) {
                console.log(`Error creating dimension: ${dimension.name}`);
                console.log(error);
                return;
            }
            console.log(`Created dimension: ${dimension.name}`);
        });
    }
}

create_default_config_keys();
create_dimensions();