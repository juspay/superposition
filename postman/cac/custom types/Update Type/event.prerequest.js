// any prerequest js code goes here
function get_type_name() {
    const host = pm.variables.get("host");
    const org_id = pm.variables.get("org_id");
    const request = {
        url: `${host}/types`,
        method: 'GET',
        header: {
            'Content-Type': 'application/json',
            'x-tenant': 'test',
            'x-org-id': `${org_id}`
        }
    }
    pm.sendRequest(request, (error, response) => {
        if(error) {
            console.log("Failed to fetch types");
            throw error;
        }
        const resp = response.json();
        for (const element of resp.data) {
            if (element.type_name === "Integer") {
                pm.environment.set("type_name", element.type_name)
                break;
            }
        }
    });
}

get_type_name();
