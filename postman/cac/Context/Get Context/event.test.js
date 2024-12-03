const expected_context = {
    "id": pm.environment.get("context_id"),
    "value": {
        "==": [
            {
                "var": "clientId"
            },
            "tamatar"
        ]
    },
    "override_id": pm.environment.get("override_id"),
    "priority": 100,
    "override": {
        "key1": "value3"
    },
    "weight": "1"
};

pm.test("200 check", function() {
    pm.response.to.have.status(200);
})

pm.test("Context equality check", function() {
    const response = pm.response.json();
    
    delete response.created_at;
    delete response.created_by;
    delete response.last_modified_at;
    delete response.last_modified_by;

    pm.expect(JSON.stringify(response)).to.be.eq(JSON.stringify(expected_context));
});
