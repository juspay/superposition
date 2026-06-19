@api @webhook
Feature: Webhook Management
  As a developer
  I want to manage webhooks for event notifications
  So that external systems can be notified when events occur

  Background:
    Given an organisation and workspace exist

  # ── Create ─────────────────────────────────────────────────────────

  Scenario: Create a webhook
    When I create a webhook named "test-webhook-create" for event "config.update"
    Then the operation should succeed
    And the response should have webhook name "test-webhook-create"
    And the response should have webhook enabled "true"

  # ── Get ────────────────────────────────────────────────────────────

  Scenario: Get a webhook by name
    Given a webhook "test-webhook-get" exists for event "config.update"
    When I get webhook "test-webhook-get"
    Then the response should have webhook name "test-webhook-get"
    And the response should have webhook url "https://example.com/webhook"

  # ── List ───────────────────────────────────────────────────────────

  Scenario: List webhooks
    Given a webhook "test-webhook-list" exists for event "config.update"
    When I list webhooks
    Then the response should contain a list with at least 1 item

  # ── Update ─────────────────────────────────────────────────────────

  Scenario: Update a webhook
    Given a webhook "test-webhook-update" exists for event "config.update"
    When I update webhook "test-webhook-update" with description "Updated description" and enabled "false"
    Then the operation should succeed
    And the response description should be "Updated description"
    And the response should have webhook enabled "false"

  # ── Get by event ───────────────────────────────────────────────────

  Scenario: Get webhook by event
    Given a webhook "test-webhook-event" exists for event "experiment.created"
    When I get webhook by event "experiment.created"
    Then the response should have webhook name "test-webhook-event"

  # ── Delete ─────────────────────────────────────────────────────────

  Scenario: Delete a webhook
    Given a webhook "test-webhook-delete" exists for event "config.update"
    When I delete webhook "test-webhook-delete"
    Then the operation should succeed
    And getting webhook "test-webhook-delete" should fail
