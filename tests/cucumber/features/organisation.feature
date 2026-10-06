@api @organisation
Feature: Organisation Management
  As a platform administrator
  I want to manage organisations
  So that I can onboard and maintain tenant organisations

  # ── Create ────────────────────────────────────────────────────────

  Scenario: Create a new organisation
    When I create an organisation with name "tmporg" and admin email "test@gmail.com"
    Then the operation should succeed
    And the response should have an "id" property

  Scenario: Fail to create organisation with invalid input
    When I create an organisation with name "" and admin email "invalid-email"
    Then the operation should fail with error matching "Json"

  # ── Get ────────────────────────────────────────────────────────────

  Scenario: Get an organisation by ID
    Given an organisation exists with name "tmporg" and admin email "test@gmail.com"
    When I get the organisation by its ID
    Then the response should have name "tmporg"
    And the response should have admin email "test@gmail.com"

  Scenario: Fail to get a non-existent organisation
    When I get an organisation with ID "non-existent-id"
    Then the operation should fail with error matching "No records found"

  Scenario: Fail to get an organisation with empty ID
    When I get an organisation with ID ""
    Then the operation should fail with error matching "Empty value provided"

  # ── List ───────────────────────────────────────────────────────────

  Scenario: List all organisations
    Given an organisation exists with name "tmporg" and admin email "test@gmail.com"
    When I list all organisations
    Then the response should contain a list
    And the list should contain the created organisation

  Scenario: List organisations with pagination
    When I list organisations with count 1 and page 1
    Then the response should contain a list with at most 1 item

  Scenario: Fail to list organisations with negative page
    When I list organisations with count 1 and page -1
    Then the operation should fail with error matching "Page should be greater than 0"

  Scenario: Fail to list organisations with negative count
    When I list organisations with count -1 and page 0
    Then the operation should fail with error matching "Count should be greater than 0"

  # ── Update ─────────────────────────────────────────────────────────

  Scenario: Update an organisation's admin email
    Given an organisation exists with name "tmporg" and admin email "test@gmail.com"
    When I update the organisation's admin email to "updated-test@gmail.com"
    Then the response should have admin email "updated-test@gmail.com"
    And getting the organisation by ID should show admin email "updated-test@gmail.com"

  Scenario: Fail to update a non-existent organisation
    When I update organisation "non-existent-id" admin email to "test@gmail.com"
    Then the operation should fail with error matching "No records found"
