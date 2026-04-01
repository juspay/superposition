---
name: superposition-experiments
description: A/B testing and experimentation for Superposition configurations. Use when creating experiments, managing variants, controlling traffic percentages, setting up webhooks, or concluding/discarding experiments.
license: Apache-2.0
compatibility: Requires Superposition instance access (local or hosted)
metadata:
  author: juspay
  version: "1.0"
---

# Superposition Experimentation

This skill helps you run A/B tests and experiments on your Superposition configurations - safely roll out changes with statistical confidence.

## Quick Start

### Understanding Experiments

Experiments let you test different configuration values with real traffic:

```text
┌─────────────────────────────────────────────────────────────┐
│                    EXPERIMENT: checkout-ui                   │
│                                                             │
│  Context: city == "Bangalore"                               │
│  Traffic: 20% (10% per variant)                             │
│                                                             │
│  ┌─────────────────┐     ┌─────────────────┐               │
│  │ CONTROL (10%)   │     │ EXPERIMENTAL    │               │
│  │                 │     │ (10%)           │               │
│  │ checkout_flow:  │     │ checkout_flow:  │               │
│  │   "classic"     │     │   "simplified"  │               │
│  └─────────────────┘     └─────────────────┘               │
└─────────────────────────────────────────────────────────────┘
```

### Core Concepts

1. **Experiment** - A test comparing different configuration values
2. **Variant** - A specific configuration (CONTROL = current, EXPERIMENTAL = new)
3. **Context** - Which users/requests are included in the experiment
4. **Traffic Percentage** - What portion of matching traffic participates
5. **Webhooks** - Notifications for experiment lifecycle events

## Common Tasks

### 1. Create an Experiment

```bash
curl -X POST http://localhost:8080/experiments \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "name": "checkout-simplified-flow",
    "description": "Test simplified checkout UI for better conversion",
    "context": {
      "city": "Bangalore"
    },
    "variants": [
      {
        "id": "control",
        "variant_type": "CONTROL",
        "overrides": {
          "checkout_flow": "classic",
          "checkout_steps": 5
        }
      },
      {
        "id": "simplified",
        "variant_type": "EXPERIMENTAL",
        "overrides": {
          "checkout_flow": "simplified",
          "checkout_steps": 3
        }
      }
    ],
    "change_reason": "A/B test for checkout optimization"
  }'
```

### 2. Start Experiment (Ramp Traffic)

Experiments start with 0% traffic. Ramp up to begin:

```bash
curl -X PUT http://localhost:8080/experiments/checkout-simplified-flow/ramp \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "traffic_percentage": 10,
    "change_reason": "Starting with 10% traffic (5% per variant)"
  }'
```

### 3. Gradually Increase Traffic

As confidence grows, increase traffic:

```bash
# Ramp to 25%
curl -X PUT http://localhost:8080/experiments/checkout-simplified-flow/ramp \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "traffic_percentage": 25,
    "change_reason": "Increasing to 25% after positive metrics"
  }'

# Ramp to 50%
curl -X PUT http://localhost:8080/experiments/checkout-simplified-flow/ramp \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "traffic_percentage": 50,
    "change_reason": "Ramping to 50% for broader testing"
  }'
```

### 4. Conclude Experiment

When you've decided on a winner, conclude the experiment:

```bash
curl -X PUT http://localhost:8080/experiments/checkout-simplified-flow/conclude \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "chosen_variant": "simplified",
    "change_reason": "Simplified flow showed 15% conversion improvement"
  }'
```

This creates permanent overrides for the winning variant's configuration.

### 5. Discard Experiment (If Not Working)

```bash
curl -X PUT http://localhost:8080/experiments/checkout-simplified-flow/discard \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "change_reason": "Simplified flow caused confusion in user testing"
  }'
```

## Experiment Lifecycle

```text
CREATED ──ramp()──> INPROGRESS ──conclude()──> CONCLUDED
    │                   │
    │                   ├──pause()──> PAUSED
    │                   │                │
    │                   └──discard()──> DISCARDED
    │                                        ^
    └────────────────discard()───────────────┘
```

### Pause & Resume

```bash
# Pause experiment (stops traffic allocation)
curl -X PUT http://localhost:8080/experiments/checkout-simplified-flow/pause \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "change_reason": "Pausing for holiday period"
  }'

# Resume experiment
curl -X PUT http://localhost:8080/experiments/checkout-simplified-flow/resume \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "change_reason": "Resuming after holiday"
  }'
```

## Multi-Variant Experiments

Test more than two variants:

```bash
curl -X POST http://localhost:8080/experiments \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "name": "pricing-strategy-test",
    "description": "Test three pricing strategies",
    "context": {
      "city": "Delhi"
    },
    "variants": [
      {
        "id": "control",
        "variant_type": "CONTROL",
        "overrides": {
          "pricing_strategy": "standard",
          "surge_cap": 2.0
        }
      },
      {
        "id": "dynamic-pricing",
        "variant_type": "EXPERIMENTAL",
        "overrides": {
          "pricing_strategy": "dynamic",
          "surge_cap": 3.0
        }
      },
      {
        "id": "flat-pricing",
        "variant_type": "EXPERIMENTAL",
        "overrides": {
          "pricing_strategy": "flat",
          "surge_cap": 1.0
        }
      }
    ],
    "change_reason": "Test different pricing models"
  }'
```

With 30% traffic and 3 variants, each gets 10% of matching traffic.

## Webhooks

Get notified when experiments change state.

### Create Webhook

```bash
curl -X POST http://localhost:8080/webhook \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "name": "deployment-webhook",
    "description": "Trigger deployments on experiment events",
    "url": "https://your-service.com/webhook",
    "method": "POST",
    "enabled": true,
    "events": ["ExperimentStarted", "ExperimentConcluded", "ExperimentDiscarded"],
    "custom_headers": {
      "Authorization": "Bearer your-token",
      "Content-Type": "application/json"
    },
    "change_reason": "Automate deployments based on experiment lifecycle"
  }'
```

### Webhook Events

| Event | When Triggered |
|-------|----------------|
| `ExperimentCreated` | New experiment created |
| `ExperimentStarted` | Experiment starts (first ramp > 0) |
| `ExperimentInprogress` | Traffic percentage updated |
| `ExperimentUpdated` | Configuration modified |
| `ExperimentConcluded` | Winner chosen |
| `ExperimentDiscarded` | Experiment discarded |
| `ExperimentPaused` | Experiment paused |

### Webhook Payload Example

```json
{
  "event_info": {
    "webhook_event": "ExperimentConcluded",
    "time": "2024-01-15T15:45:00Z",
    "workspace_id": "test",
    "organisation_id": "localorg"
  },
  "payload": {
    "id": "checkout-simplified-flow",
    "name": "checkout-simplified-flow",
    "chosen_variant": "simplified",
    "status": "CONCLUDED",
    "traffic_percentage": 50,
    "variants": [
      {"id": "control", "variant_type": "CONTROL"},
      {"id": "simplified", "variant_type": "EXPERIMENTAL"}
    ]
  }
}
```

### Handle Webhook in Your Service

```javascript
app.post('/webhook', (req, res) => {
  const { event_info, payload } = req.body;

  switch (event_info.webhook_event) {
    case 'ExperimentStarted':
      console.log(`Experiment ${payload.name} started with ${payload.traffic_percentage}% traffic`);
      break;
    case 'ExperimentConcluded':
      console.log(`Experiment ${payload.name} concluded. Winner: ${payload.chosen_variant}`);
      applyWinningConfig(payload.chosen_variant);
      break;
    case 'ExperimentDiscarded':
      console.log(`Experiment ${payload.name} was discarded`);
      break;
  }

  res.status(200).json({ received: true });
});
```

## Experiment Groups

Group related experiments together for coordinated rollouts.

### Create Experiment Group

```bash
curl -X POST http://localhost:8080/experiment-groups \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "name": "q1-pricing-changes",
    "description": "Coordinated pricing experiments for Q1",
    "change_reason": "Group all pricing experiments together"
  }'
```

### Add Experiment to Group

When creating an experiment, specify the group:

```bash
curl -X POST http://localhost:8080/experiments \
  -H "Content-Type: application/json" \
  -H "x-org-id: localorg" \
  -H "x-workspace: test" \
  -d '{
    "name": "surge-pricing-test",
    "description": "Test surge pricing model",
    "experiment_group_id": "q1-pricing-changes",
    "context": {"city": "Delhi"},
    "variants": [...],
    "change_reason": "Part of Q1 pricing initiative"
  }'
```

## Best Practices

### Traffic Allocation

| Stage | Traffic | Duration |
|-------|---------|----------|
| Initial | 1-5% | 1-2 days |
| Validation | 10-20% | 3-5 days |
| Confidence | 25-50% | 5-7 days |
| Pre-launch | 50-90% | 2-3 days |
| Full rollout | 100% | Conclude |

### Experiment Design

1. **One variable at a time** - Change only what you're testing
2. **Clear hypothesis** - "Simplified checkout will improve conversion by 10%"
3. **Sufficient sample size** - Ensure statistical significance
4. **Segment analysis** - Check results by city, device, user type

### Context Scope

```json
// Too broad - affects all users
"context": {}

// Better - targets specific segment
"context": {"city": "Bangalore"}

// Precise - narrows to specific use case
"context": {
  "city": "Bangalore",
  "user_tier": "gold",
  "platform": "mobile"
}
```

## Related Skills

- [superposition-config](../superposition-config/) - Set up dimensions and default configs first
- [superposition-provider](../superposition-provider/) - Evaluate experiments in your app
- [superposition-api](../superposition-api/) - Full REST API reference

See [references/REFERENCE.md](references/REFERENCE.md) for complete API examples.
