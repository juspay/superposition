# Superposition Javascript Client

This directory contains the Javascript implementation of Superposition, restructured following the same patterns as Java and Python clients.

## Structure

```
clients/javascript/
├── package.json          # Root workspace configuration
├── bindings/             # Native FFI bindings (like Java's ffi/ and Python's bindings/)
│   ├── package.json
│   ├── tsconfig.json
│   ├── native-resolver.ts
│   └── native-lib/       # GitHub artifacts will be placed here
├── sdk/                  # Generated Smithy SDK (migrated from clients/generated/smithy/typescript)
│   ├── package.json
│   ├── src/
│   └── dist-*/
└── openfeature/          # OpenFeature provider (like Java's open-feature-provider/)
    ├── package.json
    ├── tsconfig.json
    ├── index.ts
    ├── superposition-provider.ts
    ├── configuration-client.ts
    ├── experimentation-client.ts
    ├── types.ts
    └── utils.ts
```

## Key Changes Made

### 1. Native Resolver (bindings/)
- **Moved** `native-resolver.ts` from the old structure into `bindings/`
- **Updated** the path resolution to look for GitHub artifacts first (like Java/Python):
  1. `native-lib/` directory (for GitHub artifacts)
  2. Platform-specific subdirectories
  3. Local build fallback
  4. System path fallback
- **Enhanced** error handling and logging
- **Added** cross-platform library detection

### 2. SDK Migration (sdk/)
- **Migrated** the entire generated Smithy TypeScript SDK from `clients/generated/smithy/typescript/`
- **Preserved** all existing functionality and generated code
- **Maintained** the same API surface

### 3. OpenFeature Provider (openfeature/)
- **Updated** imports to use the new package structure:
  - `@superposition/bindings` for native resolver
  - `@superposition/sdk` for Smithy client
- **Maintained** all existing OpenFeature provider functionality
- **Added** proper TypeScript configuration and build setup

### 4. Package Structure
- **Root workspace** configuration with npm workspaces
- **Individual packages** for each component with proper dependencies
- **Build coordination** across all packages
- **GitHub artifacts support** in package files

## Usage

From external projects, you can now install the OpenFeature provider using:

```json
{
  "dependencies": {
    "@superposition/openfeature": "github:juspay/superposition#<commit-hash>:clients/typescript/openfeature"
  }
}
```

## Building

```bash
# Install all dependencies
npm run install-all

# Build all packages
npm run build

# Clean all packages
npm run clean
```

## Architecture Benefits

This restructuring provides:

1. **Modularity**: Each component is a separate npm package
2. **GitHub Artifacts**: Native libraries are loaded from published artifacts first
3. **Consistency**: Matches Java and Python client structures
4. **Maintainability**: Clear separation of concerns
5. **Reusability**: Components can be used independently