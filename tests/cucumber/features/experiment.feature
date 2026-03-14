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
    Given an experiment "exp-conclude" exists and is ramped to 100 percent
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
