@api @context
Feature: Context Management
  As a configuration administrator
  I want to manage contexts with condition-based overrides
  So that configurations vary based on runtime conditions

  Background:
    Given an organisation and workspace exist
    And dimensions and default configs are set up for context tests

  # ── Create ─────────────────────────────────────────────────────────

  Scenario: Create a context with overrides
    When I create a context with condition "os" equals "android" and override "ctx-config-key" to "android-value"
    Then the operation should succeed
    And the response should have a context ID

  # ── Get ────────────────────────────────────────────────────────────

  Scenario: Get a context by ID
    Given a context exists with condition "os" equals "ios" and override "ctx-config-key" to "ios-value"
    When I get the context by its ID
    Then the response should include the override for "ctx-config-key"

  # ── List ───────────────────────────────────────────────────────────

  Scenario: List all contexts
    Given a context exists with condition "os" equals "web" and override "ctx-config-key" to "web-value"
    When I list all contexts
    Then the response should contain a list
    And the list should contain the created context

  # ── Update Override ────────────────────────────────────────────────

  Scenario: Update a context override
    Given a context exists with condition "os" equals "android" and override "ctx-config-key" to "old-value"
    When I update the context override for "ctx-config-key" to "new-value"
    Then the operation should succeed

  # ── Move Context ───────────────────────────────────────────────────

  Scenario: Move a context to a different condition
    Given a context exists with condition "os" equals "android" and override "ctx-config-key" to "move-value"
    When I move the context to condition "os" equals "ios"
    Then the operation should succeed

  # ── Delete ─────────────────────────────────────────────────────────

  Scenario: Delete a context
    Given a context exists with condition "os" equals "android" and override "ctx-config-key" to "delete-value"
    When I delete the context
    Then the operation should succeed

  # ── Bulk Operations ────────────────────────────────────────────────

  Scenario: Perform bulk context operations
    When I perform a bulk operation to create contexts for "os" values "android,ios"
    Then the operation should succeed

  # ── Weight Recompute ───────────────────────────────────────────────

  Scenario: Recompute context weights
    Given contexts exist for weight recompute
    When I trigger weight recomputation
    Then the operation should succeed
