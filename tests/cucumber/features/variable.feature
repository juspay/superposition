@api @variable
Feature: Variable Management
  As a developer
  I want to manage variables accessible in functions
  So that functions can use dynamic runtime values

  Background:
    Given an organisation and workspace exist

  # ── Create ─────────────────────────────────────────────────────────

  Scenario: Create a variable
    When I create a variable named "TEST_API_KEY" with value "test-key-12345"
    Then the operation should succeed
    And the response should have variable name "TEST_API_KEY"
    And the response should have variable value "test-key-12345"

  # ── Get ────────────────────────────────────────────────────────────

  Scenario: Get a variable by name
    Given a variable "GET_TEST_VAR" exists with value "test-value"
    When I get variable "GET_TEST_VAR"
    Then the response should have variable name "GET_TEST_VAR"
    And the response should have variable value "test-value"

  # ── Update ─────────────────────────────────────────────────────────

  Scenario: Update a variable's value
    Given a variable "UPDATE_TEST_VAR" exists with value "original-value"
    When I update variable "UPDATE_TEST_VAR" value to "updated-value"
    Then the response should have variable value "updated-value"

  Scenario: Update a variable's description
    Given a variable "DESC_TEST_VAR" exists with value "test-value" and description "Original description"
    When I update variable "DESC_TEST_VAR" description to "Updated description"
    Then the response description should be "Updated description"

  # ── Delete ─────────────────────────────────────────────────────────

  Scenario: Delete a variable
    Given a variable "DELETE_TEST_VAR" exists with value "test-value"
    When I delete variable "DELETE_TEST_VAR"
    Then the operation should succeed
    And getting variable "DELETE_TEST_VAR" should fail with "No records found"

  # ── Error Cases ────────────────────────────────────────────────────

  Scenario: Fail to create a duplicate variable
    Given a variable "DUP_TEST_VAR" exists with value "test-value"
    When I create a variable named "DUP_TEST_VAR" with value "different-value"
    Then the operation should fail with error matching "duplicate key"

  Scenario: Fail to get a non-existent variable
    When I get variable "NON_EXISTENT_VARIABLE"
    Then the operation should fail with error matching "No records found"

  Scenario: Fail to update a non-existent variable
    When I update variable "NON_EXISTENT_VARIABLE" value to "new-value"
    Then the operation should fail with error matching "No records found"

  Scenario: Fail to delete a non-existent variable
    When I delete variable "NON_EXISTENT_VARIABLE"
    Then the operation should fail with error matching "No records found"

  Scenario: Fail to create variable with empty name
    When I create a variable named "" with value "test-value"
    Then the operation should fail with error matching "Json deserialize error"

  Scenario Outline: Fail to create variable with invalid name pattern
    When I create a variable named "<invalid_name>" with value "test-value"
    Then the operation should fail with error matching "Json deserialize error"

    Examples:
      | invalid_name               |
      | invalid-name-with-dashes   |
      | invalid name with spaces   |
      | invalid.name.with.dots     |
      | 123_STARTS_WITH_NUMBER     |
      | special@chars#not$allowed  |
      | lowercase_not_allowed      |
      | Mixed_Case_Name            |

  # ── Function Integration ───────────────────────────────────────────

  Scenario: Use a variable in a function and verify access
    Given a variable "API_KEY_FUNC" exists with value "secret-api-key-12345"
    And a function "test_var_func" exists that reads VARS.API_KEY_FUNC
    When I test the function "test_var_func"
    Then the function output should be true
    And the function stdout should contain "secret-api-key-12345"
