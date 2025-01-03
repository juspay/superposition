const host = pm.environment.get("host");
const token = pm.environment.get("token");

function create_default_config_keys() {
    let keys = [
        `pmTestKey1`,
        `pmTestKey2`
    ];

    for (const key of keys) {
        const options = {
            'method': 'POST',
            'url': `${host}/default-config`,
            "header": {
                'Authorization': `Bearer ${token}`,
                'x-tenant': 'test',
                'Content-Type': 'application/json'
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

function create_dimensions(dimension) {

        const options = {
            'method': 'POST',
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
                    "schema": dimension.schema, 
                    "description": dimension.description,
                    "change_reason": dimension.change_reason
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
            }).await;
}

create_default_config_keys();

const dimensions = [
    {name: "os", position: 1, schema: { type: "string", enum: ["android", "ios", "web"] } , description: "description", change_reason: "change_reason"},
    {name: "client", position: 2, schema: { type: "string", pattern: ".*" }, description: "description", change_reason: "change_reason"}
];
create_dimensions(dimensions[0]);
setTimeout(() => {
    create_dimensions(dimensions[1]);
}, 2000);