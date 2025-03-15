# Cypress E2E Tests

This directory contains Cypress end-to-end tests for Superposition APIs.

## Setup

1. Install dependencies:
```bash
npm install
```

2. Make sure the Superposition service is running locally (typically on port 8080)

## Running Tests

To run tests in headless mode:
```bash
npm run cypress:run
```

To open Cypress UI for interactive testing:
```bash
npm run cypress:open
```

## Test Structure

Tests are organized by API area:
- `cypress/integration/default_config.spec.js`: Tests for default configuration API endpoints
- Additional test files will be added for other API areas

## Configuration

Cypress configuration is stored in `cypress.config.js` in the project root.