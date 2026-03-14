@api @function
Feature: Function Management
  As a developer
  I want to manage validation and compute functions
  So that I can add custom logic to configuration handling

  Background:
    Given an organisation and workspace exist

  # ── Create ─────────────────────────────────────────────────────────

  Scenario: Create a value_validation function
    When I create a value_validation function named "test-val-func" with code that validates key "test-dimension"
    Then the operation should succeed
    And the response should have function type "VALUE_VALIDATION"

  Scenario: Create a value_compute function
    When I create a value_compute function named "test-comp-func" with code that returns computed values
    Then the operation should succeed
    And the response should have function type "VALUE_COMPUTE"

  # ── Get ────────────────────────────────────────────────────────────

  Scenario: Get a function by name
    Given a value_validation function "test-get-func" exists
    When I get function "test-get-func"
    Then the response should have function name "test-get-func"
    And the response should have function type "VALUE_VALIDATION"

  # ── List ───────────────────────────────────────────────────────────

  Scenario: List functions
    Given a value_validation function "test-list-func" exists
    When I list functions with count 10 and page 1
    Then the response should contain a list with at least 1 item

  # ── Update ─────────────────────────────────────────────────────────

  Scenario: Update a function's code
    Given a value_validation function "test-upd-func" exists
    When I update function "test-upd-func" with new validation code
    Then the operation should succeed
    And the response should have description "Updated value_validation function"

  # ── Publish ────────────────────────────────────────────────────────

  Scenario: Publish a function
    Given a value_validation function "test-pub-func" exists
    When I publish function "test-pub-func"
    Then the operation should succeed
    And the response should have a "published_at" property
    And the response should have a "published_code" property

  Scenario: Fail to publish a non-existent function
    When I publish function "non-existent-function"
    Then the operation should fail with error matching "No records found"

  # ── Error Cases ────────────────────────────────────────────────────

  Scenario: Fail to create a function with invalid code
    When I create a value_validation function named "invalid-func" with code "invalid code"
    Then the operation should fail

  Scenario: Fail to create a value_compute function with invalid return type
    When I create a value_compute function named "invalid-return" with code that returns a string
    Then the operation should fail
