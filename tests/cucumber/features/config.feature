@api @config @config_retrieval
Feature: Configuration Retrieval and Versioning
  As a developer
  I want to retrieve configurations and manage versions
  So that I can serve the right config values to my application

  Background:
    Given an organisation and workspace exist
    And a test default config exists for config retrieval

  # ── GetConfig ──────────────────────────────────────────────────────

  Scenario: Get configuration with context
    When I get the config with the test config key prefix
    Then the operation should succeed
    And the response should have a version

  Scenario: Pin workspace to a config version and verify
    Given I know the current config version
    When I pin the workspace to that config version
    And I get the config again
    Then the config version should match the pinned version
    When I unpin the workspace config version
    Then the workspace config version should be unset

  # ── ListVersions ───────────────────────────────────────────────────

  Scenario: List configuration versions
    When I list config versions with count 10 and page 1
    Then the operation should succeed
