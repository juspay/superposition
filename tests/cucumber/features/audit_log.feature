@api @audit_log
Feature: Audit Log Management
  As an administrator
  I want to view audit logs for my workspace
  So that I can track configuration changes for compliance

  Background:
    Given an organisation and workspace exist

  # ── List ───────────────────────────────────────────────────────────

  Scenario: List audit logs
    When I list audit logs with count 10 and page 1
    Then the operation should succeed
    And the response should contain a list

  Scenario: List audit logs filtered by INSERT action
    When I list audit logs filtered by action "INSERT"
    Then the operation should succeed

  Scenario: List audit logs filtered by table name
    When I list audit logs filtered by table "default_configs"
    Then the operation should succeed
