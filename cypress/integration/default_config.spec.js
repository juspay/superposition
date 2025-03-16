/// <reference types="cypress" />

describe('Default Config API Tests', () => {
  const apiUrl = '/default-config';
  const testKey = 'test_config_key';
  const validSchema = {
    "type": "object",
    "properties": {
      "name": { "type": "string" },
      "age": { "type": "number" }
    },
    "required": ["name"]
  };
  const validValue = {
    "name": "John Doe",
    "age": 30
  };

  const headers = {
    'Content-Type': 'application/json',
    'X-Org-Id': 'localorg',
    'X-Tenant': 'dev',
  };

  // Each test should run with a clean database state
  beforeEach(() => {
    // Clean up any existing test configs
    cy.request({
      method: 'GET',
      headers: headers,
      url: apiUrl,
      failOnStatusCode: false
    }).then(response => {
      if (response.status === 200) {
        // If test_config_key exists from previous tests, delete it
        const configs = response.body.data;
        const testConfig = configs.find(config => config.key === testKey);

        if (testConfig) {
          cy.request({
            method: 'DELETE',
            url: `${apiUrl}/${testKey}`,
            headers: headers,
            failOnStatusCode: false
          });
        }
      }
    });
  });

  it('should create a new default config', () => {
    cy.request({
      method: 'POST',
      url: apiUrl,
      headers: headers,
      body: {
        key: testKey,
        value: validValue,
        schema: validSchema,
        description: 'Test configuration',
        change_reason: 'Create test config via Cypress'
      }
    }).then(response => {
      expect(response.status).to.eq(200);
      expect(response.body).to.have.property('key', testKey);
      expect(response.body).to.have.property('value');
      expect(response.body).to.have.property('schema');
      expect(response.body).to.have.property('description', 'Test configuration');
      expect(response.headers).to.have.property('x-config-version');
    });
  });

  it.skip('should fail to create a default config with invalid schema', () => {
    const invalidSchema = {
      // Invalid schema - missing type property
      "properties": {
        "name": { "foo": "string" }
      }
    };

    cy.request({
      method: 'POST',
      url: apiUrl,
      headers: headers,
      body: {
        key: testKey,
        value: validValue,
        schema: invalidSchema,
        description: 'Test configuration',
        change_reason: 'Create test config via Cypress'
      },
      failOnStatusCode: false
    }).then(response => {
      expect(response.status).to.eq(500);
      expect(response.body).to.have.property('message');
    });
  });

  it('should fail to create a default config with invalid value', () => {
    const invalidValue = {
      // Missing required 'name' field according to schema
      "age": 30
    };

    cy.request({
      method: 'POST',
      url: apiUrl,
      headers: headers,
      body: {
        key: testKey,
        value: invalidValue,
        schema: validSchema,
        description: 'Test configuration',
        change_reason: 'Create test config via Cypress'
      },
      failOnStatusCode: false
    }).then(response => {
      expect(response.status).to.eq(400);
      expect(response.body).to.have.property('message');
      expect(response.body.message).to.include('Schema validation failed');
    });
  });

  it('should list all default configs with pagination', () => {
    // First create a test config
    cy.request({
      method: 'POST',
      url: apiUrl,
      headers: headers,
      body: {
        key: testKey,
        value: validValue,
        schema: validSchema,
        description: 'Test configuration',
        change_reason: 'Create test config via Cypress'
      }
    });

    // Then list configs
    cy.request({
      method: 'GET',
      headers: headers,
      url: `${apiUrl}?page=1&count=10`
    }).then(response => {
      expect(response.status).to.eq(200);
      expect(response.body).to.have.property('data');
      expect(response.body).to.have.property('total_pages');
      expect(response.body).to.have.property('total_items');
      expect(response.body.data).to.be.an('array');

      // Verify our test config is in the list
      const testConfig = response.body.data.find(config => config.key === testKey);
      expect(testConfig).to.not.be.undefined;
    });
  });

  it('should filter configs by name', () => {
    // First create a test config
    cy.request({
      method: 'POST',
      url: apiUrl,
      headers: headers,
      body: {
        key: testKey,
        value: validValue,
        schema: validSchema,
        description: 'Test configuration',
        change_reason: 'Create test config via Cypress'
      }
    });

    // Then list configs with a filter
    cy.request({
      method: 'GET',
      headers: headers,
      url: `${apiUrl}?name=${testKey.substring(0, 4)}`
    }).then(response => {
      expect(response.status).to.eq(200);
      expect(response.body.data).to.be.an('array');

      // Our test config should be in the filtered results
      const testConfig = response.body.data.find(config => config.key === testKey);
      expect(testConfig).to.not.be.undefined;
    });
  });

  it('should update an existing default config', () => {
    // First create a test config
    cy.request({
      method: 'POST',
      url: apiUrl,
      headers: headers,
      body: {
        key: testKey,
        value: validValue,
        schema: validSchema,
        description: 'Test configuration',
        change_reason: 'Create test config via Cypress'
      }
    });

    // Then update it
    const updatedValue = {
      "name": "Jane Smith",
      "age": 28
    };

    cy.request({
      method: 'PUT',
      url: `${apiUrl}/${testKey}`,
      headers: headers,
      body: {
        value: updatedValue,
        description: 'Updated test configuration',
        change_reason: 'Update test config via Cypress'
      }
    }).then(response => {
      expect(response.status).to.eq(200);
      expect(response.body).to.have.property('key', testKey);
      expect(response.body.value).to.deep.equal(updatedValue);
      expect(response.body).to.have.property('description', 'Updated test configuration');
      expect(response.headers).to.have.property('x-config-version');
    });
  });

  it('should delete a default config', () => {
    // First create a test config
    cy.request({
      method: 'POST',
      url: apiUrl,
      headers: headers,
      body: {
        key: testKey,
        value: validValue,
        schema: validSchema,
        description: 'Test configuration',
        change_reason: 'Create test config via Cypress'
      }
    });

    // Then delete it
    cy.request({
      method: 'DELETE',
      url: `${apiUrl}/${testKey}`,
      headers: headers
    }).then(response => {
      expect(response.status).to.eq(204);
      expect(response.headers).to.have.property('x-config-version');
    });

    // Verify it's deleted
    cy.request({
      method: 'GET',
      headers: headers,
      url: apiUrl
    }).then(response => {
      expect(response.status).to.eq(200);
      const testConfig = response.body.data.find(config => config.key === testKey);
      expect(testConfig).to.be.undefined;
    });
  });

  it.skip('should fail to delete a config in use by contexts', () => {
    // This test needs a context setup first that uses the config key
    // For now we're just checking the API response format for a simulated failure

    // Create a test config
    cy.request({
      method: 'POST',
      url: apiUrl,
      headers: headers,
      body: {
        key: testKey,
        value: validValue,
        schema: validSchema,
        description: 'Test configuration',
        change_reason: 'Create test config via Cypress'
      }
    });

    // Mock the context dependency (this is a simplified example)
    // In a real test, you would create an actual context that uses this config
    cy.intercept('DELETE', `${apiUrl}/${testKey}`, {
      statusCode: 400,
      body: {
        message: `Given key already in use in contexts: context1,context2`
      }
    }).as('deleteRequest');


    cy.request({
      method: 'DELETE',
      url: `${apiUrl}/${testKey}`,
      headers: headers,
      failOnStatusCode: false
    }).then(response => {
      cy.wait('@deleteRequest');
      expect(response.status).to.eq(400);
      expect(response.body).to.have.property('message');
      expect(response.body.message).to.include('already in use in contexts');
    });

  });
});
