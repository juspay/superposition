# Superposition Core Clients - TypeScript

This package contains the core TypeScript clients for Superposition, including the resolver client with native FFI bindings and OpenFeature provider integration.

## What's Included

- **ConfigurationClient**: Client for configuration resolution with caching
- **ExperimentationClient**: Client for experiment evaluation and variant selection
- **SuperpositionProvider**: OpenFeature-compatible provider for feature flag evaluation
- **NativeResolver**: FFI bindings to the native Superposition core library
- **Examples**: Usage examples showing different configuration patterns

## Installation

```bash
npm install @superposition/core-clients-typescript
```

## Quick Start

### Using the Builder Pattern

```typescript
import { superpositionProvider } from '@superposition/core-clients-typescript';
import { OpenFeature } from '@openfeature/server-sdk';

const provider = superpositionProvider()
    .endpoint('http://localhost:8080')
    .workspaceId('dev')
    .orgId('localorg')
    .token('your-token')
    .refreshStrategy({
        polling: {
            enabled: true,
            interval: 30000,
            onError: 'stale'
        }
    })
    .experimentationOptions({
        enabled: true
    })
    .build();

await OpenFeature.setProvider(provider);
const client = OpenFeature.getClient();

// Evaluate flags
const featureFlag = await client.getBooleanValue('my-feature', false, { userId: '123' });
```

### Direct Configuration

```typescript
import { SuperpositionProvider } from '@superposition/core-clients-typescript';

const provider = new SuperpositionProvider({
    superpositionConfig: {
        endpoint: 'http://localhost:8080',
        workspace_id: 'dev',
        org_id: 'localorg',
        token: { token: 'your-token' }
    },
    configOptions: {
        refreshStrategy: {
            polling: {
                enabled: true,
                interval: 30000,
                onError: 'stale'
            }
        }
    },
    experimentationOptions: {
        enabled: true
    }
});
```

## Architecture

This package is structured to provide clean separation between:

- **Core clients** (ConfigurationClient, ExperimentationClient)
- **OpenFeature integration** (SuperpositionProvider)
- **Native bindings** (NativeResolver with FFI)
- **Type definitions** (All interfaces and types)

## Building

```bash
npm run build
```

## Examples

See the `examples/` directory for comprehensive usage examples showing different configuration patterns and use cases.

## Migration from Generated Smithy Client

If you were previously using the resolver client from the generated Smithy TypeScript SDK, update your imports:

```typescript
// Before
import { SuperpositionProvider } from '../src/resolver-client/superposition-provider';

// After
import { SuperpositionProvider } from '@superposition/core-clients-typescript';
```

The API remains the same, only the import path has changed.