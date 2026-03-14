@api @secret
Feature: Secret Management
  As a developer
  I want to manage encrypted secrets accessible in functions
  So that sensitive values are securely stored and used

  Background:
    Given an organisation and workspace exist

  # ── Create ─────────────────────────────────────────────────────────

  Scenario: Create a secret
    When I create a secret named "TEST_API_KEY_SECRET" with value "test-key-12345"
    Then the operation should succeed
    And the response should have secret name "TEST_API_KEY_SECRET"

  # ── Get ────────────────────────────────────────────────────────────

  Scenario: Get a secret by name (value should not be returned)
    Given a secret "GET_TEST_SECRET" exists with value "test-value"
    When I get secret "GET_TEST_SECRET"
    Then the response should have secret name "GET_TEST_SECRET"
    And the secret value should not be returned

  # ── Update and Verify ──────────────────────────────────────────────

  Scenario: Verify secret value update via function
    Given a secret "UPDATE_VERIFY_SECRET" exists with value "original-secret-value"
    And a compute function exists that reads the secret "UPDATE_VERIFY_SECRET"
    When I test the compute function
    Then the function output should contain "original-secret-value"
    When I update secret "UPDATE_VERIFY_SECRET" value to "updated-secret-value"
    And I test the compute function again
    Then the function output should contain "updated-secret-value"

  # ── Delete ─────────────────────────────────────────────────────────

  Scenario: Delete a secret
    Given a secret "DELETE_TEST_SECRET" exists with value "test-value"
    When I delete secret "DELETE_TEST_SECRET"
    Then the operation should succeed
    And getting secret "DELETE_TEST_SECRET" should fail with "No records found"

  # ── Error Cases ────────────────────────────────────────────────────

  Scenario: Fail to create a duplicate secret
    Given a secret "DUP_TEST_SECRET" exists with value "test-value"
    When I create a secret named "DUP_TEST_SECRET" with value "different-value"
    Then the operation should fail with error matching "duplicate key"

  Scenario: Fail to get a non-existent secret
    When I get secret "NON_EXISTENT_SECRET"
    Then the operation should fail with error matching "No records found"

  Scenario: Fail to update a non-existent secret
    When I update secret "NON_EXISTENT_SECRET" value to "new-value"
    Then the operation should fail with error matching "No records found"

  Scenario: Fail to delete a non-existent secret
    When I delete secret "NON_EXISTENT_SECRET"
    Then the operation should fail with error matching "No records found"

  Scenario: Fail to create secret with empty name
    When I create a secret named "" with value "test-value"
    Then the operation should fail with error matching "Parse error"

  Scenario Outline: Fail to create secret with invalid name pattern
    When I create a secret named "<invalid_name>" with value "test-value"
    Then the operation should fail with error matching "Parse error"

    Examples:
      | invalid_name               |
      | invalid-name-with-dashes   |
      | invalid name with spaces   |
      | invalid.name.with.dots     |
      | 123_STARTS_WITH_NUMBER     |
      | special@chars#not$allowed  |
      | lowercase_not_allowed      |
      | Mixed_Case_Name            |
