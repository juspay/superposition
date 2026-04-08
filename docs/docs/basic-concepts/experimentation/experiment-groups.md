---
sidebar_position: 2
title: Experiment Groups
description: Group experiments to prevent overlap and ensure isolated traffic allocation
---

## Introduction

When running multiple experiments, they can overlap with each other due to the cascading nature of [Context-Aware Configs](/docs/basic-concepts/how-all-this-works.mdx). This overlap makes it difficult to predict the final configuration a user receives, as multiple experiments may simultaneously modify the same config keys.

**Experiment Groups** solve this by ensuring experiments within a group never overlap with each other. Each user is assigned to exactly one experiment (or none) within the group, providing predictable and isolated traffic allocation.

:::info Key Insight
Experiments inside a group are isolated from each other. Experiments outside the group can still overlap with experiments inside the group — isolation only applies within the group.
:::

## Core Concepts

### Experiment Group

An experiment group is a container for experiments with the following properties:

| Property | Description |
|----------|-------------|
| `name` | Unique identifier for the group |
| `description` | Human-readable description |
| `context` | The context that defines which traffic is eligible for this group |
| `traffic_percentage` | Percentage of matching traffic (0-100%) that enters the group |
| `member_experiment_ids` | List of experiment IDs belonging to this group |
| `group_type` | Either `USER_CREATED` or `SYSTEM_GENERATED` |

### Group Types

- **USER_CREATED**: Groups created manually by users. Members can be added/removed.
- **SYSTEM_GENERATED**: Groups created automatically when an experiment is created with an `experiment_group_id`. These cannot be modified directly.

### Context Constraint

Experiments added to a group must have a context that **contains** the group's context. This ensures experiments are at least as specific as the group.

For example, if a group has context `[city IS "Bangalore"]`, member experiments must have contexts like:
- `[city IS "Bangalore", hour_of_day IS "8"]` ✓
- `[city IS "Bangalore"]` ✓
- `[city IS "Mumbai"]` ✗ (doesn't contain the group context)

## How It Works: Bucket Mechanism

The isolation between experiments in a group is achieved through a **bucket-based allocation system**.

### 100 Buckets

Each experiment group maintains exactly 100 buckets, where each bucket represents 1% of traffic. A bucket can either be:
- **Empty** (no experiment assigned)
- **Assigned** (contains `experiment_id` and `variant_id`)

### Deterministic Assignment

When a user requests configuration, the system:

1. **Hash Calculation**: Computes `hash(identifier, group_id) % 100` to get a bucket index (0-99)
2. **Context Check**: Verifies the user's context matches the group's context
3. **Traffic Check**: Verifies the bucket index is within the group's `traffic_percentage`
4. **Bucket Lookup**: Returns the experiment/variant assigned to that bucket

```
User ID: "user-123"
Group ID: 456
Hash: hash("user-123", 456) % 100 = 42

If traffic_percentage >= 43 and bucket[42] is assigned:
    → User gets the experiment/variant in bucket[42]
Else:
    → User is not part of any experiment in this group
```

### Why This Ensures No Overlap

Since each bucket can only hold one `(experiment_id, variant_id)` pair, a user falling into bucket 42 will only ever receive one specific variant of one experiment. This guarantees experiments in the group never overlap for the same user.

### Bucket Allocation Example

Consider a group with `traffic_percentage = 30%` and two experiments:

| Experiment | Traffic | Variants | Buckets Needed |
|------------|---------|----------|----------------|
| Exp A      | 10%     | 2        | 20 buckets (10% × 2 variants) |
| Exp B      | 5%      | 2        | 10 buckets (5% × 2 variants) |

The 30 buckets (0-29) are divided among experiments, with each variant getting an equal share. Buckets 30-99 remain empty.


## Use Cases

### Parallel Feature Tests

Run multiple A/B tests for different features without worrying about interaction effects:

```
Group: "Q1-Mobile-Experiments"
├── Experiment: Button Color Test (10% traffic)
├── Experiment: Checkout Flow Test (5% traffic)
└── Experiment: Payment Options Test (8% traffic)
```

Each user in the group sees exactly one of these experiments.

### Isolated Testing Environments

Create groups with specific contexts to isolate experiments for different segments:

```
Group A: context = [environment IS "staging"]
Group B: context = [environment IS "production"]
```

## Best Practices

1. **Start Small**: Begin with lower traffic percentages and increase gradually
2. **Context Alignment**: Ensure experiment contexts properly contain the group context
3. **Monitor Bucket Allocation**: The sum of (traffic_percentage × variant_count) for all experiments should not exceed the group's traffic_percentage
4. **Use Meaningful Names**: Group names should reflect the experiments' purpose or theme
5. **Document Changes**: Always provide clear `change_reason` for auditability

## Related Concepts

- [Experiments](/docs/basic-concepts/experimentation/experiments) - Understanding individual experiments
- [Context-Aware Configs](/docs/basic-concepts/context-aware-configs) - The foundation for experimentation