pm.test("200 check", function() {
    pm.response.to.have.status(200);
    let response = pm.response.json();
    let expected_response = {
        "contexts": [],
        "overrides": {},
        "default_configs": {"key1": "value1"}
    };
    pm.expect(JSON.stringify(response)).to.be.eq(JSON.stringify(expected_response));
})
