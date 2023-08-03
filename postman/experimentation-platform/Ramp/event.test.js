const host = pm.environment.get("host");
const token = pm.environment.get("token");

function fetch_experiment_n_test(experiment_id, expected_traffic_percentage) {
    const options = {
        'method': 'GET',
        'url': `${host}/experiments/${experiment_id}`,
        "header": {
            'Authorization': `Bearer ${token}`,
            'Content-Type': 'application/json'
        }
    };

    pm.sendRequest(options, function(error, response) {
        if(error) {
            console.log("Failed to fetch experiment");
            throw error;
        }
        
        const experiment = response.json();
        console.log(`Expected: ${expected_traffic_percentage}, Actual: ${experiment.traffic_percentage}`);
        pm.expect(experiment.traffic_percentage).to.be.eq(expected_traffic_percentage);
    });
}

// check experiment creation in experiment
pm.test("200 OK", function () {
    pm.response.to.have.status(200);
});


// check for contexts in CAC
pm.test("Test traffic percentage", function() {
    const experiment_id = pm.environment.get("experiment_id");

    fetch_experiment_n_test(experiment_id, 46);
});