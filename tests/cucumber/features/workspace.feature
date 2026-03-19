@api @workspace
Feature: Workspace Management
  As a platform administrator
  I want to manage workspaces within an organisation
  So that I can isolate configuration environments

  Background:
    Given an organisation exists for workspace tests

  # ── List ───────────────────────────────────────────────────────────

  Scenario: List workspaces
    When I list workspaces with count 10 and page 1
    Then the response should contain a workspace list
    And the response should have a "total_items" count

  # ── Create ─────────────────────────────────────────────────────────

  Scenario: Create a new workspace
    When I create a workspace with name "cucumbertestws" and admin email "admin@example.com"
    Then the operation should succeed
    And the response should have workspace name "cucumbertestws"
    And the response should have workspace status "ENABLED"
    And the response should have workspace admin email "admin@example.com"

  # ── Get via List ────────────────────────────────────────────────────

  Scenario: Find a created workspace in the list
    Given a workspace exists with name "cucumbertestws"
    When I list workspaces with count 100 and page 1
    Then the list should contain workspace "cucumbertestws"

  # ── Update ─────────────────────────────────────────────────────────

  Scenario: Update workspace admin email
    Given a workspace exists with name "cucumbertestws"
    When I update workspace "cucumbertestws" admin email to "updated-admin@example.com"
    Then the response should have workspace admin email "updated-admin@example.com"

  # ── Filters ────────────────────────────────────────────────────────

  Scenario: List workspaces filtered by ENABLED status
    When I list workspaces filtered by status "ENABLED"
    Then all returned workspaces should have status "ENABLED"

  # ── Error Cases ────────────────────────────────────────────────────

  Scenario: List workspaces with invalid organisation ID returns empty
    When I list workspaces for organisation "non-existent-org"
    Then the operation should succeed

  Scenario: Fail to create workspace with invalid data
    When I create a workspace with name "" and admin email "invalid-email"
    Then the operation should fail

  Scenario: Fail to create workspace with special characters
    When I create a workspace with name "test-special-chars@!#" and admin email "admin@example.com"
    Then the operation should fail
