@api @type_template
Feature: Type Template Management
  As a configuration administrator
  I want to manage reusable type templates
  So that dimensions and configs can reference shared schema types

  Background:
    Given an organisation and workspace exist

  # ── List ───────────────────────────────────────────────────────────

  Scenario: List type templates
    When I list type templates
    Then the operation should succeed
    And the response should contain a type template list

  # ── Create ─────────────────────────────────────────────────────────

  Scenario: Create a Boolean type template
    When I create a type template named "Boolean" with schema type "boolean"
    Then the operation should succeed
    And the response should have type name "Boolean"
    And the response schema type should be "boolean"

  Scenario: Create a Pattern type template
    When I create a type template named "Pattern" with pattern ".*"
    Then the operation should succeed
    And the response schema should have pattern ".*"

  # ── Update ─────────────────────────────────────────────────────────

  Scenario: Update a type template with range constraints
    Given a type template "Decimal" exists with schema type "number"
    When I update type template "Decimal" with minimum 0 and maximum 100
    Then the response schema should have minimum 0 and maximum 100

  # ── Delete ─────────────────────────────────────────────────────────

  Scenario: Delete a type template
    Given a type template "ToDelete" exists with schema type "boolean"
    When I delete type template "ToDelete"
    Then the operation should succeed
    And listing type templates should not include "ToDelete"

  # ── Error Cases ────────────────────────────────────────────────────

  Scenario: Fail to create a type template with invalid schema
    When I create a type template named "Invalid" with schema type "invalid"
    Then the operation should fail
