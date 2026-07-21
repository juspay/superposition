---
slug: context-aware-vendure-custom-fields
title: Context-aware Vendure custom fields with Superposition
description: How Superposition adds channel aware resolved values to Vendure custom fields.
tags: [superposition, vendure, commerce, configuration]
---

import AnimationSlide from '@site/src/components/AnimationSlide';
import { VendureOwnershipFlow } from '@site/src/components/animations';

Commerce data is rarely as global as it first appears.

An online store often needs extra product information: a price band, a fulfilment promise, a merchandising label, or a marketplace-specific badge.
In Vendure, custom fields are a natural place to store this data because they let teams extend products and product variants directly.

But some values are "default" values.

The effective value can change by channel, currency, market, customer group, product, variant, or a rollout state.
That is where things usually start to get messy: more custom fields, more conditional code, more duplicated entities, more operational risk.

The [Superposition Vendure plugin](https://github.com/juspay/vendure-plugin-superposition) takes a cleaner approach:

```text
Vendure owns the stored fallback.
Superposition owns the context-aware effective value
```

<AnimationSlide width="100%" height={420} mode="auto" loop={false}>
    <VendureOwnershipFlow />
</AnimationSlide>

Vendure remains the system of record for the catalog. Superposition adds a runtime configuration layer that can resolve the right value for the current business context.

<!-- truncate -->

## Why Superposition fits this problem

Superposition starts from a simple idea: configuration should be resolved in context. Instead of spreading rules across environment variables, hardcoded conditionals , or one-off feature flags, it lets teams define typed defaults and resolve the right value for each context in a cascading way.

The important part is that the catalog does not need to become more complex just because the business rules become more specific. Vendure keeps the base product data understandable, and Superposition resolves the effective value at read time.

## A concrete example

Suppose a merchant wants to store a `bulkPrice` for each product variant. In Vendure, this can be added as a custom field on `ProductVariant`.

```ts
SuperpositionPlugin.init({
    entities: {
        ProductVariant: [{ name: 'bulkPrice', type: 'int', nullable: true }],
    },
});
```

Vendure stores the fallback value:

```text
ProductVariant.customFields.bulkPrice = 800
```

That value is still useful. It is the base price that applies when no more specific rule is present.

Now suppose the same merchant wants a different bulk price for the B2B channel in INR. That value does not need to become another Vendure custom field. It can be stored as a contextual override in Superposition:

```text
channel = b2b
currency = INR
entity = product_variant
entity_id = 1
bulkPrice = 1200
```

At read time, the plugin builds a context from the request and the entity being resolved. If the context matches the override, Superposition returns `1200`. If it does not, the value falls back to the Vendure custom field value, `800`.

The storefront can ask for both values:

```graphql
query {
    productVariant(id: "1") {
        id
        customFields {
            bulkPrice
        }
        resolvedBulkPrice
    }
}
```

The distinction is intentional. `customFields.bulkPrice` is the value stored in Vendure. `resolvedBulkPrice` is the value resolved for the current context.

For the default context, the resolved value is `800`. For the B2B INR context, the resolved value is `1200`.

## Why not replace the custom field?

The plugin does not overwrite Vendure's nested `customFields.bulkPrice` field. That field remains the persisted fallback value. This is important because merchants and developers already expect Vendure custom fields to represent data stored in Vendure.

Instead, the plugin exposes a separate resolved field:

```text
customFields.bulkPrice = stored fallback
resolvedBulkPrice = context-aware value
```

This keeps ownership clear. Vendure remains responsible for the base catalog value, while Superposition is responsible for resolving the effective value for a request.

## How it works

The integration follows the same shape as the example.

First, the plugin registers the configured custom fields with Vendure. That lets Vendure keep storing the fallback value on the entity, using the normal custom-field mechanism.

Second, the plugin adds resolved fields to the GraphQL API. For a custom field named `bulkPrice`, the API can expose a field such as `resolvedBulkPrice`.

Third, when a resolved field is requested, the plugin builds a Superposition context from the current request and entity. That context can include values such as the active channel, currency, entity type, and entity id.

```text
channel = current channel
currency = current currency
entity = product_variant
entity_id = current variant id
```

Superposition then evaluates that context against the configured overrides and returns the effective value. If no override applies, the plugin returns the Vendure fallback value.

## What this gives teams

The most immediate benefit is that teams avoid turning every contextual rule into a new catalog field. A merchant does not need separate fields like `b2bBulkPrice`, `inrBulkPrice`, or `enterpriseBulkPrice`. The catalog can keep one field, `bulkPrice`, while Superposition decides which value applies for the current request.

It also keeps operational changes out of application code. A price band, label, or availability note can change for a market or channel without adding another conditional branch to the storefront or redeploying the Vendure application.

And because Superposition treats these values as typed configuration, teams get a cleaner place to manage overrides, review changes, and reason about why a request received a particular value.

## Dashboard behavior

The Dashboard extension shows a read-only Superposition values panel on product and product-variant detail pages. The goal is to make the relationship visible: the merchant can see the value stored in Vendure and the value resolved by Superposition for the current context.

It does not replace Vendure's built-in custom-field editor. Editing the fallback value in Vendure and editing a contextual override in Superposition are different operations, so the plugin keeps those responsibilities separate.

## A clean extension point

This leaves room for richer workflows without changing the meaning of the existing fields. For example, an override editor can be added later to create or update Superposition values from the Dashboard. That editor would still be separate from Vendure's built-in custom-field form, because it would be editing a contextual override rather than the stored fallback.

The core idea stays the same: Vendure owns the catalog value, and Superposition resolves the effective value for the context.

## Closing thoughts

The plugin is intentionally small in concept. It does not replace Vendure's catalog model, and it does not ask teams to move product data out of Vendure. It adds a context-aware layer around the places where commerce data naturally varies.

That makes custom fields more useful in real-world commerce setups, where the right value often depends on channel, currency, market, customer segment, or rollout state.

You can find the plugin and usage examples in the [juspay/vendure-plugin-superposition](https://github.com/juspay/vendure-plugin-superposition) repository.
