pm.test("304 check", function() {
    pm.response.to.have.status(304);
})