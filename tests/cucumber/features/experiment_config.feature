@api @config @experiment_config
Feature: Experiment Configuration Retrieval
  As a developer
  I want to fetch experiment-specific configurations
  So that I can understand which experiments are active and their variants

  Background:
    Given an organisation and workspace exist
    And dimensions and default configs are set up for experiment tests
    And an experiment "exp-config" exists and is ramped to 50 percent

  # ── GetExperimentConfig ────────────────────────────────────────────

  Scenario: Get experiment configuration
    When I get the experiment config
    Then the operation should succeed
    And the response should have an "experiments" property

  Scenario: Get experiment config with prefix filter
    When I get the experiment config with prefix "exp-config-key"
    Then the operation should succeed

  Scenario: Get experiment config with context filter
    When I get the experiment config with context "os" equals "android"
    Then the operation should succeed

  # ── ApplicableVariants ─────────────────────────────────────────────

  Scenario: Get applicable variants for a context
    When I get applicable variants with context "os" equals "android" and identifier "test-user-1"
    Then the operation should succeed
