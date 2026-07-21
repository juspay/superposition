@api @config @resolve_config
Feature: Config Resolution with Identifier Bucketing
  As a developer
  I want to resolve configurations using identifiers
  So that users are consistently bucketed into experiment variants

  Background:
    Given an organisation and workspace exist
    And a dimension, default config, and experiment are set up for bucketing tests

  Scenario: Resolve config with identifier returns bucketed value
    When I resolve the config with the test identifier and matching context
    Then the operation should succeed
    And the response should have a version
    And the config value should be either the default or experimental value
