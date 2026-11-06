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

  # ── Get ────────────────────────────────────────────────────────────

  Scenario: Get a single workspace by name
    Given a workspace exists with name "cucumbertestws"
    When I get workspace "cucumbertestws"
    Then the operation should succeed
    And the response should have workspace name "cucumbertestws"

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

  # ── Metrics ────────────────────────────────────────────────────────

  Scenario: Create a workspace with metric definitions
    When I create a workspace "metricsws" with metric definitions "conversion:maximize,latency:minimize"
    Then the operation should succeed
    And the workspace metrics should be enabled
    And the workspace metric definitions should include "conversion" with direction "maximize"
    And the workspace metric definitions should include "latency" with direction "minimize"
    And the workspace should have 2 metric definitions

  Scenario: Update a workspace's metric definitions
    Given a workspace exists with name "metricsupd"
    When I update workspace "metricsupd" metric definitions to "retention:maximize"
    Then the operation should succeed
    And the workspace metrics should be enabled
    And the workspace metric definitions should include "retention" with direction "maximize"
    And the workspace should have 1 metric definition

  Scenario: Fail to create a workspace with duplicate metric names
    When I create a workspace "metricsdup" with metric definitions "conversion:maximize,conversion:minimize"
    Then the operation should fail with error matching "duplicate metric name"

  Scenario: Fail to create a workspace with metrics enabled but no source or definitions
    When I create a workspace "metricsempty" with metrics enabled and no definitions
    Then the operation should fail with error matching "must be provided when metrics is enabled"

  Scenario: Fail to create a workspace with an empty metric name
    When I create a workspace "metricsblank" with metric definitions ":maximize"
    Then the operation should fail with error matching "Empty value not allowed"

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
