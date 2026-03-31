# Experimentation API Reference

Complete API reference for A/B testing, experiments, webhooks, and experiment groups.

## Table of Contents

- [Experiments](#experiments)
- [Traffic Management](#traffic-management)
- [Experiment Lifecycle](#experiment-lifecycle)
- [Webhooks](#webhooks)
- [Experiment Groups](#experiment-groups)

---

## Experiments

### Create Experiment

**POST /experiments**

Creates a new experiment with variants and context.

```bash
curl -X POST http://localhost:8080/experiments \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "name": "homepage-hero-test",
    "description": "Test new hero section design",
    "context": {
      "platform": "web"
    },
    "variants": [
      {
        "id": "control",
        "variant_type": "CONTROL",
        "overrides": {
          "hero_title": "Welcome to Our App",
          "hero_cta": "Get Started",
          "hero_layout": "classic"
        }
      },
      {
        "id": "new-design",
        "variant_type": "EXPERIMENTAL",
        "overrides": {
          "hero_title": "Experience the Future",
          "hero_cta": "Start Free Trial",
          "hero_layout": "modern"
        }
      }
    ],
    "change_reason": "Testing new hero section for conversion optimization"
  }'
```

**Response:**
```json
{
  "id": "homepage-hero-test",
  "name": "homepage-hero-test",
  "description": "Test new hero section design",
  "context": {"platform": "web"},
  "status": "CREATED",
  "traffic_percentage": 0,
  "override_keys": ["hero_title", "hero_cta", "hero_layout"],
  "variants": [
    {
      "id": "control",
      "variant_type": "CONTROL",
      "overrides": {
        "hero_title": "Welcome to Our App",
        "hero_cta": "Get Started",
        "hero_layout": "classic"
      }
    },
    {
      "id": "new-design",
      "variant_type": "EXPERIMENTAL",
      "overrides": {
        "hero_title": "Experience the Future",
        "hero_cta": "Start Free Trial",
        "hero_layout": "modern"
      }
    }
  ],
  "created_at": "2024-01-15T10:30:00Z",
  "created_by": "admin",
  "last_modified": "2024-01-15T10:30:00Z",
  "last_modified_by": "admin",
  "change_reason": "Testing new hero section for conversion optimization"
}
```

### Multi-Variant Experiment

```bash
curl -X POST http://localhost:8080/experiments \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "name": "pricing-tiers-test",
    "description": "Test three different pricing tier structures",
    "context": {
      "platform": "web",
      "user_type": "new"
    },
    "variants": [
      {
        "id": "control",
        "variant_type": "CONTROL",
        "overrides": {
          "pricing_tier_basic": 9.99,
          "pricing_tier_pro": 29.99,
          "pricing_tier_enterprise": 99.99
        }
      },
      {
        "id": "lower-prices",
        "variant_type": "EXPERIMENTAL",
        "overrides": {
          "pricing_tier_basic": 7.99,
          "pricing_tier_pro": 24.99,
          "pricing_tier_enterprise": 79.99
        }
      },
      {
        "id": "freemium",
        "variant_type": "EXPERIMENTAL",
        "overrides": {
          "pricing_tier_basic": 0.00,
          "pricing_tier_pro": 19.99,
          "pricing_tier_enterprise": 89.99
        }
      }
    ],
    "change_reason": "Testing price sensitivity with new users"
  }'
```

### Experiment with Complex Context

```bash
curl -X POST http://localhost:8080/experiments \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "name": "premium-feature-rollout",
    "description": "Roll out premium features to select users",
    "context": {
      "city": "Bangalore",
      "user_tier": "gold",
      "platform": "mobile"
    },
    "variants": [
      {
        "id": "control",
        "variant_type": "CONTROL",
        "overrides": {
          "show_premium_badge": false,
          "premium_features": []
        }
      },
      {
        "id": "treatment",
        "variant_type": "EXPERIMENTAL",
        "overrides": {
          "show_premium_badge": true,
          "premium_features": ["early_access", "priority_support"]
        }
      }
    ],
    "change_reason": "Testing premium feature adoption with loyal mobile users"
  }'
```

### List Experiments

**GET /experiments**

```bash
curl -X GET http://localhost:8080/experiments \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

**Query Parameters:**
- `status` - Filter by status (CREATED, INPROGRESS, CONCLUDED, DISCARDED, PAUSED)
- `limit` - Number of results to return
- `offset` - Pagination offset

```bash
curl -X GET "http://localhost:8080/experiments?status=INPROGRESS" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

**Response:**
```json
{
  "data": [
    {
      "id": "homepage-hero-test",
      "name": "homepage-hero-test",
      "status": "INPROGRESS",
      "traffic_percentage": 25,
      "override_keys": ["hero_title", "hero_cta", "hero_layout"],
      "created_at": "2024-01-15T10:30:00Z",
      "created_by": "admin"
    }
  ],
  "total": 1
}
```

### Get Experiment

**GET /experiments/{experiment_id}**

```bash
curl -X GET http://localhost:8080/experiments/homepage-hero-test \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

**Response:**
```json
{
  "id": "homepage-hero-test",
  "name": "homepage-hero-test",
  "description": "Test new hero section design",
  "context": {"platform": "web"},
  "status": "INPROGRESS",
  "traffic_percentage": 25,
  "override_keys": ["hero_title", "hero_cta", "hero_layout"],
  "variants": [
    {
      "id": "control",
      "variant_type": "CONTROL",
      "context_id": "ctx-abc",
      "override_id": "ovr-123",
      "overrides": {
        "hero_title": "Welcome to Our App",
        "hero_cta": "Get Started",
        "hero_layout": "classic"
      }
    },
    {
      "id": "new-design",
      "variant_type": "EXPERIMENTAL",
      "context_id": "ctx-def",
      "override_id": "ovr-456",
      "overrides": {
        "hero_title": "Experience the Future",
        "hero_cta": "Start Free Trial",
        "hero_layout": "modern"
      }
    }
  ],
  "created_at": "2024-01-15T10:30:00Z",
  "created_by": "admin",
  "last_modified": "2024-01-20T14:00:00Z",
  "last_modified_by": "admin",
  "started_at": "2024-01-16T09:00:00Z",
  "started_by": "admin",
  "description": "Test new hero section design",
  "change_reason": "Testing new hero section for conversion optimization"
}
```

### Get Applicable Variants

**POST /experiments/applicable-variants**

Returns which variant applies for a given context (used by providers).

```bash
curl -X POST http://localhost:8080/experiments/applicable-variants \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "context": {
      "platform": "web",
      "user_id": "user-123"
    }
  }'
```

**Response:**
```json
{
  "data": [
    {
      "experiment_id": "homepage-hero-test",
      "variant_id": "new-design",
      "variant_type": "EXPERIMENTAL",
      "overrides": {
        "hero_title": "Experience the Future",
        "hero_cta": "Start Free Trial",
        "hero_layout": "modern"
      }
    }
  ]
}
```

---

## Traffic Management

### Ramp Traffic

**PUT /experiments/{experiment_id}/ramp**

Increase or decrease experiment traffic percentage.

```bash
curl -X PUT http://localhost:8080/experiments/homepage-hero-test/ramp \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "traffic_percentage": 25,
    "change_reason": "Increasing traffic after positive initial results"
  }'
```

**Response:**
```json
{
  "id": "homepage-hero-test",
  "name": "homepage-hero-test",
  "status": "INPROGRESS",
  "traffic_percentage": 25,
  ...
}
```

### Traffic Percentage Guidelines

| Action | Traffic % | Description |
|--------|-----------|-------------|
| Start | 1-5% | Initial exposure for safety checks |
| Validate | 10-20% | Gather preliminary metrics |
| Confidence | 25-50% | Build statistical significance |
| Pre-launch | 50-90% | Final validation before full rollout |
| Full | 100% | All matching traffic included |

### Weight Recompute

**POST /experiments/{experiment_id}/weight-recompute**

Recalculates variant weights after configuration changes.

```bash
curl -X POST http://localhost:8080/experiments/homepage-hero-test/weight-recompute \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "change_reason": "Recomputing weights after config update"
  }'
```

---

## Experiment Lifecycle

### Conclude Experiment

**PUT /experiments/{experiment_id}/conclude**

Ends the experiment and applies the winning variant permanently.

```bash
curl -X PUT http://localhost:8080/experiments/homepage-hero-test/conclude \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "chosen_variant": "new-design",
    "change_reason": "New design showed 15% conversion improvement (p < 0.05)"
  }'
```

**What happens on conclude:**
1. Experiment status changes to CONCLUDED
2. Winning variant's overrides become permanent contexts
3. Losing variant's overrides are removed
4. Experiment webhook fires if configured

**Response:**
```json
{
  "id": "homepage-hero-test",
  "name": "homepage-hero-test",
  "status": "CONCLUDED",
  "chosen_variant": "new-design",
  "traffic_percentage": 0,
  ...
}
```

### Discard Experiment

**PUT /experiments/{experiment_id}/discard**

Abandons the experiment without applying any changes.

```bash
curl -X PUT http://localhost:8080/experiments/homepage-hero-test/discard \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "change_reason": "Experiment showed negative impact on conversion"
  }'
```

**What happens on discard:**
1. Experiment status changes to DISCARDED
2. All experiment contexts/overrides are removed
3. Default configuration values remain unchanged
4. Experiment webhook fires if configured

### Pause Experiment

**PUT /experiments/{experiment_id}/pause**

Temporarily stops traffic allocation to the experiment.

```bash
curl -X PUT http://localhost:8080/experiments/homepage-hero-test/pause \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "change_reason": "Pausing for weekend traffic analysis"
  }'
```

**Response:**
```json
{
  "id": "homepage-hero-test",
  "name": "homepage-hero-test",
  "status": "PAUSED",
  "traffic_percentage": 25,
  ...
}
```

### Resume Experiment

**PUT /experiments/{experiment_id}/resume**

Resumes a paused experiment.

```bash
curl -X PUT http://localhost:8080/experiments/homepage-hero-test/resume \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "change_reason": "Resuming after analysis"
  }'
```

### Update Experiment

**PATCH /experiments/{experiment_id}**

Updates experiment metadata or variant overrides.

```bash
curl -X PATCH http://localhost:8080/experiments/homepage-hero-test \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "description": "Updated description with learnings",
    "change_reason": "Adding documentation from initial results"
  }'
```

### Update Variant Overrides

```bash
curl -X PATCH http://localhost:8080/experiments/homepage-hero-test \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "variants": [
      {
        "id": "new-design",
        "variant_type": "EXPERIMENTAL",
        "overrides": {
          "hero_title": "Experience the Future",
          "hero_cta": "Start Free Trial",
          "hero_layout": "modern",
          "hero_subtitle": "Join millions of users"  // Added
        }
      }
    ],
    "change_reason": "Adding subtitle based on feedback"
  }'
```

---

## Webhooks

### Create Webhook

**POST /webhook**

```bash
curl -X POST http://localhost:8080/webhook \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "name": "experiment-lifecycle-webhook",
    "description": "Send Slack notifications for experiment events",
    "url": "https://hooks.slack.com/services/your/webhook/url",
    "method": "POST",
    "enabled": true,
    "events": ["ExperimentStarted", "ExperimentConcluded", "ExperimentDiscarded"],
    "custom_headers": {
      "Content-Type": "application/json"
    },
    "change_reason": "Team notifications for experiment lifecycle"
  }'
```

**Events:**
- `ExperimentCreated` - New experiment created
- `ExperimentStarted` - First ramp with traffic > 0
- `ExperimentInprogress` - Traffic percentage changed
- `ExperimentUpdated` - Experiment configuration modified
- `ExperimentConcluded` - Winner chosen
- `ExperimentDiscarded` - Experiment abandoned
- `ExperimentPaused` - Experiment paused

**Response:**
```json
{
  "name": "experiment-lifecycle-webhook",
  "description": "Send Slack notifications for experiment events",
  "url": "https://hooks.slack.com/services/your/webhook/url",
  "method": "POST",
  "enabled": true,
  "events": ["ExperimentStarted", "ExperimentConcluded", "ExperimentDiscarded"],
  "custom_headers": {
    "Content-Type": "application/json"
  },
  "created_at": "2024-01-15T10:30:00Z",
  "created_by": "admin",
  "change_reason": "Team notifications for experiment lifecycle"
}
```

### List Webhooks

**GET /webhook**

```bash
curl -X GET http://localhost:8080/webhook \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Get Webhook

**GET /webhook/{webhook_name}**

```bash
curl -X GET http://localhost:8080/webhook/experiment-lifecycle-webhook \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Update Webhook

**PATCH /webhook/{webhook_name}**

```bash
curl -X PATCH http://localhost:8080/webhook/experiment-lifecycle-webhook \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "enabled": false,
    "change_reason": "Temporarily disabling for maintenance"
  }'
```

### Delete Webhook

**DELETE /webhook/{webhook_name}**

```bash
curl -X DELETE http://localhost:8080/webhook/experiment-lifecycle-webhook \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Get Webhooks by Event

**GET /webhook/event/{event_type}**

```bash
curl -X GET http://localhost:8080/webhook/event/ExperimentConcluded \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Webhook Payload Format

All webhook payloads follow this structure:

```json
{
  "event_info": {
    "webhook_event": "ExperimentStarted",
    "time": "2024-01-15T10:30:00Z",
    "workspace_id": "test",
    "organisation_id": "localorg",
    "config_version": "12345"
  },
  "payload": {
    // Event-specific experiment data
  }
}
```

### Webhook Payload Examples

**ExperimentStarted:**
```json
{
  "event_info": {
    "webhook_event": "ExperimentStarted",
    "time": "2024-01-15T10:30:00Z",
    "workspace_id": "test",
    "organisation_id": "localorg"
  },
  "payload": {
    "id": "homepage-hero-test",
    "name": "homepage-hero-test",
    "context": {"platform": "web"},
    "variants": [
      {"id": "control", "variant_type": "CONTROL"},
      {"id": "new-design", "variant_type": "EXPERIMENTAL"}
    ],
    "traffic_percentage": 10,
    "status": "INPROGRESS"
  }
}
```

**ExperimentConcluded:**
```json
{
  "event_info": {
    "webhook_event": "ExperimentConcluded",
    "time": "2024-01-20T15:45:00Z",
    "workspace_id": "test",
    "organisation_id": "localorg"
  },
  "payload": {
    "id": "homepage-hero-test",
    "name": "homepage-hero-test",
    "chosen_variant": "new-design",
    "variants": [
      {"id": "control", "variant_type": "CONTROL"},
      {"id": "new-design", "variant_type": "EXPERIMENTAL"}
    ],
    "status": "CONCLUDED"
  }
}
```

**ExperimentDiscarded:**
```json
{
  "event_info": {
    "webhook_event": "ExperimentDiscarded",
    "time": "2024-01-18T11:20:00Z",
    "workspace_id": "test",
    "organisation_id": "localorg"
  },
  "payload": {
    "id": "pricing-tiers-test",
    "name": "pricing-tiers-test",
    "status": "DISCARDED"
  }
}
```

---

## Experiment Groups

Experiment groups organize related experiments together.

### Create Experiment Group

**POST /experiment-groups**

```bash
curl -X POST http://localhost:8080/experiment-groups \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "name": "q1-2024-optimization",
    "description": "Q1 2024 product optimization initiatives",
    "change_reason": "Grouping all Q1 optimization experiments"
  }'
```

**Response:**
```json
{
  "id": "grp-q1-2024-optimization",
  "name": "q1-2024-optimization",
  "description": "Q1 2024 product optimization initiatives",
  "created_at": "2024-01-15T10:30:00Z",
  "created_by": "admin",
  "change_reason": "Grouping all Q1 optimization experiments"
}
```

### List Experiment Groups

**GET /experiment-groups**

```bash
curl -X GET http://localhost:8080/experiment-groups \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Get Experiment Group

**GET /experiment-group/{group_id}**

```bash
curl -X GET http://localhost:8080/experiment-group/grp-q1-2024-optimization \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Update Experiment Group

**PATCH /experiment-group/{group_id}**

```bash
curl -X PATCH http://localhost:8080/experiment-group/grp-q1-2024-optimization \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "description": "Updated description with specific goals",
    "change_reason": "Adding KPIs and success criteria"
  }'
```

### Delete Experiment Group

**DELETE /experiment-group/{group_id}**

```bash
curl -X DELETE http://localhost:8080/experiment-group/grp-q1-2024-optimization \
  -H "x-org-id: localorg" \
  -H "x-workspace: test"
```

### Add Experiment to Group

When creating or updating an experiment:

```bash
curl -X POST http://localhost:8080/experiments \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "name": "checkout-optimization",
    "experiment_group_id": "q1-2024-optimization",
    "context": {"platform": "web"},
    "variants": [...],
    "change_reason": "Part of Q1 optimization group"
  }'
```

---

## Status Reference

| Status | Description |
|--------|-------------|
| `CREATED` | Initial state after creation, 0% traffic |
| `INPROGRESS` | Actively running with traffic > 0 |
| `PAUSED` | Temporarily stopped, retains traffic setting |
| `CONCLUDED` | Completed with winner applied permanently |
| `DISCARDED` | Abandoned, no changes applied |

---

## Best Practices

### Traffic Ramp Schedule

```bash
# Day 1: Safety check
curl -X PUT .../ramp -d '{"traffic_percentage": 5, "change_reason": "Initial safety check"}'

# Day 3: Validation
curl -X PUT .../ramp -d '{"traffic_percentage": 20, "change_reason": "Validation phase"}'

# Day 7: Confidence building
curl -X PUT .../ramp -d '{"traffic_percentage": 50, "change_reason": "Building confidence"}'

# Day 12: Pre-launch
curl -X PUT .../ramp -d '{"traffic_percentage": 90, "change_reason": "Final validation"}'

# Day 14: Conclude
curl -X PUT .../conclude -d '{"chosen_variant": "treatment", "change_reason": "Positive results"}'
```
