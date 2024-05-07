// any prerequest js code goes here
function get_context_id() {
    const host = pm.variables.get("host");
    const request = {
        url: `${host}/types`,
        method: 'GET',
        header: {
            'Content-Type': 'application/json',
            'x-tenant': 'test',
        }
    }
    pm.sendRequest(request, (error, response) => {
        if(error) {
            console.log("Failed to fetch types");
            throw error;
        }
        const resp = response.json();
        for (const element of resp) {
            if (element.display_name === "Integer") {
                pm.environment.set("id", element.id)
                break;
            }
        }
    });
}

get_context_id();