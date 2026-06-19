@api @dimension
Feature: Dimension Management
  As a configuration administrator
  I want to manage dimensions (context keys)
  So that I can define the axes along which configuration varies

  Background:
    Given an organisation and workspace exist

  # ── Create ─────────────────────────────────────────────────────────

  Scenario: Create a string dimension
    When I create a dimension with name "test-dim-str" and schema type "string"
    Then the operation should succeed
    And the response should have dimension name "test-dim-str"

  Scenario: Create an enum dimension
    When I create a dimension with name "test-dim-enum" and enum values "small,big,otherwise"
    Then the operation should succeed

  Scenario: Create a boolean dimension
    When I create a dimension with name "test-dim-bool" and schema type "boolean"
    Then the operation should succeed

  # ── Get ────────────────────────────────────────────────────────────

  Scenario: Get a dimension by name
    Given a dimension "test-dim-get" exists with schema type "string"
    When I get dimension "test-dim-get"
    Then the response should have dimension name "test-dim-get"

  # ── List ───────────────────────────────────────────────────────────

  Scenario: List all dimensions
    Given a dimension "test-dim-list" exists with schema type "string"
    When I list all dimensions
    Then the response should contain a list
    And the list should contain dimension "test-dim-list"

  # ── Update ─────────────────────────────────────────────────────────

  Scenario: Update a dimension's description
    Given a dimension "test-dim-upd" exists with schema type "string"
    When I update dimension "test-dim-upd" description to "Updated description"
    Then the operation should succeed

  # ── Delete ─────────────────────────────────────────────────────────

  Scenario: Delete a dimension
    Given a dimension "test-dim-del" exists with schema type "string"
    When I delete dimension "test-dim-del"
    Then the operation should succeed

  # ── Error Cases ────────────────────────────────────────────────────

  Scenario: Fail to create a dimension with invalid schema
    When I create a dimension with name "test-dim-invalid" and schema type "invalid-type"
    Then the operation should fail
