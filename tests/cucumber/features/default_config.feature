@api @config @default_config
Feature: Default Configuration Management
  As a developer
  I want to manage default configurations with schema validation
  So that my application has well-defined configuration defaults

  Background:
    Given an organisation and workspace exist
    And validation functions are set up

  # ── Create ─────────────────────────────────────────────────────────

  Scenario: Create a valid default config
    When I create a default config with key "test-key" and value:
      | name      | Test User |
      | age       | 30        |
    And the schema requires "name" as string and "age" as number with minimum 0
    Then the operation should succeed

  Scenario: Fail to create config with invalid schema type
    When I create a default config with key "test-key-2" and an invalid schema type "invalid-type"
    Then the operation should fail with error matching "Invalid JSON schema"

  Scenario: Fail to create config with empty schema
    When I create a default config with key "test-key-2" and an empty schema
    Then the operation should fail with error matching "Schema cannot be empty"

  Scenario: Fail to create config when value violates schema constraints
    When I create a default config with key "test-key-2" where age is -5 but minimum is 0
    Then the operation should fail with error matching "value is too small, minimum is 0"

  Scenario: Fail to create config when function validation fails
    When I create a default config with key "test-key-2" using validation function "false_validation"
    Then the operation should fail with error matching "validation failed"

  Scenario: Create config with value_compute function
    When I create a default config with key "test-key-3" using compute function "auto_fn"
    Then the operation should succeed
    And the response should have value_compute_function_name "auto_fn"

  Scenario: Fail to create config with non-existent function
    When I create a default config with key "test-key-2" using validation function "non_existent_function"
    Then the operation should fail with error matching "published code does not exist"

  # ── Update ─────────────────────────────────────────────────────────

  Scenario: Update an existing default config value
    Given a default config exists with key "test-key-upd" and value "Test User" age 30
    When I update the default config "test-key-upd" with value "Updated User" age 35
    Then the response value should have name "Updated User" and age 35

  Scenario: Update schema and value together
    Given a default config exists with key "test-key-upd2" and value "Test User" age 30
    When I update default config "test-key-upd2" schema to add email field and set value with email "updated@example.com"
    Then the response value should include email "updated@example.com"

  Scenario: Fail to update a non-existent key
    When I update default config "non_existent_key" with a new value
    Then the operation should fail with error matching "No record found"

  Scenario: Fail to update with invalid schema
    Given a default config exists with key "test-key-upd3" and value "Test User" age 30
    When I update default config "test-key-upd3" schema to an invalid type
    Then the operation should fail with error matching "Invalid JSON schema"

  Scenario: Fail to update when value misses required field
    Given a default config exists with key "test-key-upd4" and requires name and email
    When I update default config "test-key-upd4" value without the required email field
    Then the operation should fail with error matching "required property"

  Scenario: Update config with a validation function
    Given a default config exists with key "test-key-upd5" and value "Test User" age 30
    When I update default config "test-key-upd5" validation function to "true_function"
    Then the response should have value_validation_function_name "true_function"
