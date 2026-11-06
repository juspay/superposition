@api @experiment
Feature: Experiment Management
  As a product manager
  I want to manage A/B test experiments
  So that I can test configuration variants with controlled traffic

  Background:
    Given an organisation and workspace exist
    And dimensions and default configs are set up for experiment tests

  # ── Create ─────────────────────────────────────────────────────────

  Scenario: Create an experiment with control and experimental variants
    When I create an experiment with name "exp-test" and context "os" equals "android"
    And the experiment has a control variant with override "exp-config-key" = "control-val"
    And the experiment has an experimental variant with override "exp-config-key" = "experimental-val"
    Then the operation should succeed
    And the response should have experiment status "CREATED"
    And the response should have 2 variants

  # ── Get ────────────────────────────────────────────────────────────

  Scenario: Get an experiment by ID
    Given an experiment "exp-get" exists with context "os" equals "ios"
    When I get the experiment by its ID
    Then the response should have the experiment name
    And the response should have experiment status "CREATED"

  # ── List ───────────────────────────────────────────────────────────

  Scenario: List experiments
    Given an experiment "exp-list" exists with context "os" equals "android"
    When I list experiments
    Then the response should contain a list
    And the list should contain the created experiment

  # ── Ramp ───────────────────────────────────────────────────────────

  Scenario: Ramp an experiment to 50% traffic
    Given an experiment "exp-ramp" exists with context "os" equals "android"
    When I ramp the experiment to 50 percent traffic
    Then the operation should succeed
    And the experiment status should be "INPROGRESS"

  # ── Update Overrides ───────────────────────────────────────────────

  Scenario: Update experiment variant overrides
    Given an experiment "exp-override" exists with context "os" equals "android"
    When I update the experimental variant override for "exp-config-key" to "updated-val"
    Then the operation should succeed

  # ── Conclude ───────────────────────────────────────────────────────

  Scenario: Conclude an experiment
    Given an experiment "exp-conclude" exists and is ramped to 50 percent
    When I conclude the experiment with the experimental variant
    Then the experiment status should be "CONCLUDED"

  # ── Discard ────────────────────────────────────────────────────────

  Scenario: Discard a created experiment
    Given an experiment "exp-discard" exists with context "os" equals "ios"
    When I discard the experiment
    Then the experiment status should be "DISCARDED"

  Scenario: Discard an in-progress experiment
    Given an experiment "exp-discard-prog" exists and is ramped to 50 percent
    When I discard the experiment
    Then the experiment status should be "DISCARDED"

  # ── Metrics ────────────────────────────────────────────────────────

  Scenario: Create an experiment with a metric selection from the workspace definitions
    Given the workspace has metric definitions "conversion:maximize,latency:minimize,errors:minimize"
    When I create an experiment "exp-metrics" with primary metric "conversion", secondary metric "latency" and guardrail "errors"
    Then the operation should succeed
    And the experiment metrics should be enabled
    And the experiment primary metric should be "conversion" with direction "maximize"
    And the experiment secondary metric should be "latency"
    And the experiment guardrail metric should be "errors"

  Scenario: Create an experiment with only the required primary and guardrail metrics
    Given the workspace has metric definitions "conversion:maximize,errors:minimize"
    When I create an experiment "exp-metrics-min" with primary metric "conversion" and guardrail "errors"
    Then the operation should succeed
    And the experiment primary metric should be "conversion" with direction "maximize"
    And the experiment should have no secondary metric
    And the experiment guardrail metric should be "errors"

  Scenario: Fail to create an experiment selecting a metric not defined in the workspace
    Given the workspace has metric definitions "conversion:maximize,errors:minimize"
    When I create an experiment "exp-metrics-bad" with primary metric "revenue" and guardrail "errors"
    Then the operation should fail with error matching "not defined in the workspace"

  Scenario: Fail to create an experiment with a metric selection when the workspace has no metric list
    Given the workspace has no metric definitions
    When I create an experiment "exp-metrics-nolist" with primary metric "conversion" and guardrail "errors"
    Then the operation should fail with error matching "metric list configured"

  Scenario: Update an experiment's metric selection while it is in the created state
    Given the workspace has metric definitions "conversion:maximize,latency:minimize,errors:minimize"
    And an experiment "exp-metrics-upd" exists with context "os" equals "android"
    When I update the experiment metric selection to primary "latency" and guardrail "errors"
    Then the operation should succeed
    And the experiment metrics should be enabled
    And the experiment primary metric should be "latency" with direction "minimize"

  Scenario: An experiment created without metrics inherits disabled metrics from a workspace without a source
    Given the workspace has no metric definitions
    And an experiment "exp-metrics-none" exists with context "os" equals "android"
    When I get the experiment by its ID
    Then the experiment metrics should be disabled

  # ── Pause / Resume ────────────────────────────────────────────────

  Scenario: Pause an active experiment
    Given an experiment exists and is ramped
    When I pause the experiment
    Then the operation should succeed

  Scenario: Resume a paused experiment
    Given an experiment exists and is paused
    When I resume the experiment
    Then the operation should succeed
