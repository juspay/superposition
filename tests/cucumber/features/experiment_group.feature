@api @experiment @experiment_group
Feature: Experiment Group Management
  As a product manager
  I want to group related experiments together
  So that I can manage traffic and lifecycle for experiment sets

  Background:
    Given an organisation and workspace exist
    And dimensions and default configs are set up for experiment group tests
    And experiments are set up for group tests

  # ── Create ─────────────────────────────────────────────────────────

  Scenario: Create an experiment group with valid members
    When I create an experiment group with name "test-group" and member experiments
    Then the operation should succeed
    And the response should contain the member experiment IDs
    And the response traffic percentage should be 100

  Scenario: Create an experiment group with no members
    When I create an experiment group with name "empty-group" and no members
    Then the operation should succeed
    And the response member list should be empty

  Scenario: Fail to create group with in-progress experiment
    When I create an experiment group including an in-progress experiment
    Then the operation should fail with error matching "not in the created stage"

  Scenario: Fail to create group with conflicting context experiment
    When I create an experiment group including an experiment with conflicting context
    Then the operation should fail with error matching "contexts do not match"

  Scenario: Fail to create group with traffic percentage over 100
    When I create an experiment group with traffic percentage 101
    Then the operation should fail

  # ── Get ────────────────────────────────────────────────────────────

  Scenario: Get an experiment group by ID
    Given an experiment group exists
    When I get the experiment group by its ID
    Then the operation should succeed
    And the response should have a group name

  Scenario: Fail to get a non-existent experiment group
    When I get an experiment group with ID "123"
    Then the operation should fail with error matching "No records found"

  # ── Update ─────────────────────────────────────────────────────────

  Scenario: Update experiment group traffic percentage
    Given an experiment group exists
    When I update the experiment group traffic percentage to 75
    Then the response traffic percentage should be 75

  Scenario: Update experiment group description
    Given an experiment group exists
    When I update the experiment group description to "Updated description"
    Then the response description should be "Updated description"

  Scenario: Fail to update a non-existent group
    When I update experiment group "123" traffic percentage to 50
    Then the operation should fail with error matching "No records found"

  # ── Add/Remove Members ─────────────────────────────────────────────

  Scenario: Add members to an experiment group
    Given an experiment group exists with no members
    When I add a valid experiment to the group
    Then the response should contain the added experiment ID

  Scenario: Remove members from an experiment group
    Given an experiment group exists with members
    When I remove a member from the group
    Then the response should not contain the removed experiment ID

  Scenario: Fail to add an in-progress experiment to a group
    Given an experiment group exists
    When I add an in-progress experiment to the group
    Then the operation should fail with error matching "not in the created stage"

  # ── List ───────────────────────────────────────────────────────────

  Scenario: List experiment groups
    Given an experiment group exists
    When I list experiment groups
    Then the response should contain a list
    And the list should contain the created group

  Scenario: List experiment groups sorted by created_at descending
    When I list experiment groups sorted by "created_at" in "DESC" order
    Then the response should be sorted by created_at descending

  # ── Delete ─────────────────────────────────────────────────────────

  Scenario: Fail to delete a group with active members
    Given an experiment group exists with members
    When I delete the experiment group
    Then the operation should fail with error matching "has members"

  Scenario: Fail to delete a non-existent group
    When I delete experiment group "22"
    Then the operation should fail with error matching "No records found"
