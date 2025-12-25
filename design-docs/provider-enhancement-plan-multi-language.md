# Multi-Language Provider Enhancement Plan

## Overview

This document outlines the implementation plan for the Superposition provider refactor across Java, JavaScript/TypeScript, and Python. The goal is to maintain **interface and capability parity** with the Rust implementation while following each language's idioms and best practices.

## Core Architecture (Language-Agnostic)

All implementations follow this unified architecture:

```
┌─────────────────────────────────────────────────────────┐
│                   Interface Layer                       │
│  • AllFeatureProvider (bulk config resolution)          │
│  • FeatureExperimentMeta (experiment metadata)          │
│  • SuperpositionDataSource (data source abstraction)    │
└─────────────────────────────────────────────────────────┘
                            │
        ┌───────────────────┴───────────────────┐
        │                                       │
┌───────▼────────┐                    ┌────────▼──────────────┐
│  Data Sources  │                    │    LocalResolution    │
│  • HTTP        │                    │    Provider           │
│  • File+Watch  │                    │  Uses core evaluation │
│    (CAC TOML)  │                    │  for resolution       │
└────────────────┘                    └───────────────────────┘
```

## Key Principles

1. **Interface Consistency**: All languages expose identical methods with same semantics
2. **Data Format Compatibility**: JSON serialization for cross-language interoperability
3. **Async-First**: All I/O operations are asynchronous where language supports it
4. **Thread-Safe**: All implementations are safe for concurrent use
5. **CAC TOML Support**: File-based configuration using CAC TOML format
6. **OpenFeature Integration**: Compatible with OpenFeature SDK in each language

---

# Java Implementation Plan

## Technology Stack

- **Language**: Java 17+ (LTS)
- **Async Framework**: CompletableFuture / Project Reactor (reactive streams)
- **HTTP Client**: OkHttp / Java 11+ HttpClient
- **File Watching**: WatchService (java.nio.file)
- **TOML Parser**: toml4j or jackson-dataformat-toml
- **JSON**: Jackson or Gson
- **OpenFeature**: OpenFeature Java SDK
- **Build Tool**: Maven or Gradle

## Module Structure

```
superposition-provider/
├── pom.xml / build.gradle
├── README.md
├── src/
│   ├── main/
│   │   └── java/
│   │       └── com/juspay/superposition/provider/
│   │           ├── interfaces/
│   │           │   ├── AllFeatureProvider.java
│   │           │   ├── FeatureExperimentMeta.java
│   │           │   └── SuperpositionDataSource.java
│   │           ├── model/
│   │           │   ├── AllFeatureProviderMetadata.java
│   │           │   ├── ExperimentMeta.java
│   │           │   ├── ConfigData.java
│   │           │   └── ExperimentData.java
│   │           ├── datasource/
│   │           │   ├── HttpDataSource.java
│   │           │   └── FileDataSource.java
│   │           ├── providers/
│   │           │   ├── LocalResolutionProvider.java
│   │           │   └── LocalResolutionProviderOptions.java
│   │           ├── types/
│   │           │   ├── RefreshStrategy.java
│   │           │   ├── PollingStrategy.java
│   │           │   └── OnDemandStrategy.java
│   │           └── utils/
│   │               ├── CacTomlParser.java
│   │               └── ExpressionParser.java
│   ├── test/
│   │   └── java/
│   │       └── com/juspay/superposition/provider/
│   │           ├── LocalResolutionProviderTest.java
│   │           ├── HttpDataSourceTest.java
│   │           └── FileDataSourceTest.java
│   └── resources/
│       └── test-data/
│           └── example.cac.toml
└── examples/
    ├── LocalHttpExample.java
    ├── LocalFileExample.java
    ├── LocalFileWatchExample.java
    └── AllFeaturesExample.java
```

## Core Interfaces

### AllFeatureProvider Interface

```java
package com.juspay.superposition.provider.interfaces;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import com.juspay.superposition.provider.model.AllFeatureProviderMetadata;

/**
 * Interface for bulk configuration resolution
 */
public interface AllFeatureProvider {
    /**
     * Resolve all features for the given evaluation context
     *
     * @param context Evaluation context
     * @return CompletableFuture with map of feature keys to values
     */
    CompletableFuture<Map<String, Object>> resolveAllFeatures(
        Map<String, Object> context
    );

    /**
     * Resolve all features matching the given prefix filters
     *
     * @param context Evaluation context
     * @param prefixFilter List of prefixes to filter by (null for no filtering)
     * @return CompletableFuture with filtered map of features
     */
    CompletableFuture<Map<String, Object>> resolveAllFeaturesWithFilter(
        Map<String, Object> context,
        List<String> prefixFilter
    );

    /**
     * Get metadata about this provider
     *
     * @return Provider metadata
     */
    AllFeatureProviderMetadata getMetadata();
}
```

### FeatureExperimentMeta Interface

```java
package com.juspay.superposition.provider.interfaces;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import com.juspay.superposition.provider.model.ExperimentMeta;

/**
 * Interface for experiment metadata and variant resolution
 */
public interface FeatureExperimentMeta {
    /**
     * Get all applicable variant IDs for the given context
     */
    CompletableFuture<List<String>> getApplicableVariants(
        Map<String, Object> context
    );

    /**
     * Get detailed experiment metadata for the given context
     */
    CompletableFuture<List<ExperimentMeta>> getExperimentMetadata(
        Map<String, Object> context
    );

    /**
     * Get the variant for a specific experiment
     *
     * @return Optional variant ID
     */
    CompletableFuture<Optional<String>> getExperimentVariant(
        String experimentId,
        Map<String, Object> context
    );
}
```

### SuperpositionDataSource Interface

```java
package com.juspay.superposition.provider.interfaces;

import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import com.juspay.superposition.provider.model.ConfigData;
import com.juspay.superposition.provider.model.ExperimentData;

/**
 * Interface for abstracting data sources
 */
public interface SuperpositionDataSource {
    /**
     * Fetch the latest configuration from the data source
     */
    CompletableFuture<ConfigData> fetchConfig();

    /**
     * Fetch experiment data from the data source
     *
     * @return Optional experiment data (empty if not supported)
     */
    CompletableFuture<Optional<ExperimentData>> fetchExperiments();

    /**
     * Get a human-readable name for this data source
     */
    String getSourceName();

    /**
     * Check if this data source supports experiments
     */
    boolean supportsExperiments();

    /**
     * Close and cleanup resources used by this data source
     */
    CompletableFuture<Void> close();
}
```

## Implementation Details

### LocalResolutionProvider

```java
package com.juspay.superposition.provider.providers;

import java.time.Instant;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;
import com.juspay.superposition.provider.interfaces.*;
import com.juspay.superposition.provider.model.*;

public class LocalResolutionProvider implements
    AllFeatureProvider,
    FeatureExperimentMeta {

    private final AllFeatureProviderMetadata metadata;
    private final SuperpositionDataSource dataSource;
    private final LocalResolutionProviderOptions options;

    // Thread-safe caches using AtomicReference
    private final AtomicReference<Config> cachedConfig;
    private final AtomicReference<Experiments> cachedExperiments;
    private final AtomicReference<ExperimentGroups> cachedExperimentGroups;
    private final AtomicReference<Instant> lastConfigUpdate;
    private final AtomicReference<Instant> lastExperimentsUpdate;

    // Scheduled executor for polling
    private final ScheduledExecutorService scheduler;
    private ScheduledFuture<?> pollingTask;

    public LocalResolutionProvider(
        SuperpositionDataSource dataSource,
        LocalResolutionProviderOptions options
    ) {
        this.dataSource = dataSource;
        this.options = options;
        this.metadata = new AllFeatureProviderMetadata(
            "LocalResolutionProvider",
            getVersion()
        );

        this.cachedConfig = new AtomicReference<>();
        this.cachedExperiments = new AtomicReference<>();
        this.cachedExperimentGroups = new AtomicReference<>();
        this.lastConfigUpdate = new AtomicReference<>();
        this.lastExperimentsUpdate = new AtomicReference<>();

        this.scheduler = Executors.newScheduledThreadPool(1);
    }

    /**
     * Initialize the provider and start background tasks if needed
     */
    public CompletableFuture<Void> init() {
        return refreshConfig()
            .thenCompose(v -> {
                if (options.isEnableExperiments() && dataSource.supportsExperiments()) {
                    return refreshExperiments();
                }
                return CompletableFuture.completedFuture(null);
            })
            .thenRun(() -> {
                if (options.getRefreshStrategy() instanceof PollingStrategy) {
                    startPolling();
                }
            });
    }

    private void startPolling() {
        PollingStrategy strategy = (PollingStrategy) options.getRefreshStrategy();
        long interval = strategy.getInterval();

        pollingTask = scheduler.scheduleAtFixedRate(
            () -> {
                refreshConfig().exceptionally(e -> {
                    // Log error but continue polling
                    System.err.println("Polling error: " + e.getMessage());
                    return null;
                });

                if (options.isEnableExperiments() && dataSource.supportsExperiments()) {
                    refreshExperiments().exceptionally(e -> {
                        System.err.println("Experiment polling error: " + e.getMessage());
                        return null;
                    });
                }
            },
            interval,
            interval,
            TimeUnit.SECONDS
        );
    }

    private CompletableFuture<Void> refreshConfig() {
        return dataSource.fetchConfig()
            .thenAccept(configData -> {
                cachedConfig.set(configData.getConfig());
                lastConfigUpdate.set(configData.getFetchedAt());
            });
    }

    private CompletableFuture<Void> refreshExperiments() {
        return dataSource.fetchExperiments()
            .thenAccept(optionalData -> {
                optionalData.ifPresent(expData -> {
                    cachedExperiments.set(expData.getExperiments());
                    cachedExperimentGroups.set(expData.getExperimentGroups());
                    lastExperimentsUpdate.set(expData.getFetchedAt());
                });
            });
    }

    @Override
    public CompletableFuture<Map<String, Object>> resolveAllFeatures(
        Map<String, Object> context
    ) {
        return checkAndRefreshIfNeeded()
            .thenApply(v -> {
                Config config = cachedConfig.get();
                if (config == null) {
                    return options.getFallbackConfig().orElse(Collections.emptyMap());
                }

                // Add variant IDs to context if experiments enabled
                Map<String, Object> enhancedContext = new HashMap<>(context);
                if (options.isEnableExperiments()) {
                    List<String> variants = getApplicableVariantsSync(context);
                    enhancedContext.put("variantIds", variants);
                }

                // Call superposition_core eval_config equivalent
                return evalConfig(config, enhancedContext);
            });
    }

    @Override
    public CompletableFuture<Map<String, Object>> resolveAllFeaturesWithFilter(
        Map<String, Object> context,
        List<String> prefixFilter
    ) {
        return resolveAllFeatures(context)
            .thenApply(allFeatures -> {
                if (prefixFilter == null || prefixFilter.isEmpty()) {
                    return allFeatures;
                }

                Map<String, Object> filtered = new HashMap<>();
                for (Map.Entry<String, Object> entry : allFeatures.entrySet()) {
                    for (String prefix : prefixFilter) {
                        if (entry.getKey().startsWith(prefix)) {
                            filtered.put(entry.getKey(), entry.getValue());
                            break;
                        }
                    }
                }
                return filtered;
            });
    }

    @Override
    public AllFeatureProviderMetadata getMetadata() {
        return metadata;
    }

    // Additional helper methods...

    /**
     * Shutdown the provider and cleanup resources
     */
    public CompletableFuture<Void> shutdown() {
        if (pollingTask != null) {
            pollingTask.cancel(false);
        }
        scheduler.shutdown();
        return dataSource.close();
    }
}
```

### FileDataSource with Watch Service

```java
package com.juspay.superposition.provider.datasource;

import java.io.IOException;
import java.nio.file.*;
import java.time.Instant;
import java.util.Optional;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;
import com.juspay.superposition.provider.interfaces.SuperpositionDataSource;
import com.juspay.superposition.provider.model.*;
import com.juspay.superposition.provider.utils.CacTomlParser;

public class FileDataSource implements SuperpositionDataSource {
    private final Path configPath;
    private final boolean watchFiles;
    private final AtomicReference<ConfigData> cachedConfig;
    private final WatchService watchService;
    private final ExecutorService watchExecutor;

    public FileDataSource(Path configPath, boolean watchFiles) throws IOException {
        this.configPath = configPath;
        this.watchFiles = watchFiles;
        this.cachedConfig = new AtomicReference<>();

        // Load initial config
        loadConfig();

        if (watchFiles) {
            this.watchService = FileSystems.getDefault().newWatchService();
            Path dir = configPath.getParent();
            dir.register(watchService,
                StandardWatchEventKinds.ENTRY_MODIFY,
                StandardWatchEventKinds.ENTRY_CREATE
            );

            this.watchExecutor = Executors.newSingleThreadExecutor();
            startWatching();
        } else {
            this.watchService = null;
            this.watchExecutor = null;
        }
    }

    private void startWatching() {
        watchExecutor.submit(() -> {
            while (true) {
                try {
                    WatchKey key = watchService.take();

                    for (WatchEvent<?> event : key.pollEvents()) {
                        WatchEvent.Kind<?> kind = event.kind();

                        if (kind == StandardWatchEventKinds.OVERFLOW) {
                            continue;
                        }

                        @SuppressWarnings("unchecked")
                        WatchEvent<Path> ev = (WatchEvent<Path>) event;
                        Path filename = ev.context();

                        if (filename.equals(configPath.getFileName())) {
                            System.out.println("Config file changed, reloading...");
                            loadConfig();
                        }
                    }

                    boolean valid = key.reset();
                    if (!valid) {
                        break;
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                } catch (Exception e) {
                    System.err.println("Watch error: " + e.getMessage());
                }
            }
        });
    }

    private void loadConfig() {
        try {
            Config config = CacTomlParser.parse(configPath);
            cachedConfig.set(new ConfigData(config, Instant.now()));
        } catch (Exception e) {
            System.err.println("Failed to load config: " + e.getMessage());
        }
    }

    @Override
    public CompletableFuture<ConfigData> fetchConfig() {
        ConfigData config = cachedConfig.get();
        if (config == null) {
            return CompletableFuture.failedFuture(
                new IllegalStateException("Config not loaded")
            );
        }
        return CompletableFuture.completedFuture(config);
    }

    @Override
    public CompletableFuture<Optional<ExperimentData>> fetchExperiments() {
        // File source doesn't support experiments
        return CompletableFuture.completedFuture(Optional.empty());
    }

    @Override
    public String getSourceName() {
        return "FileDataSource(" + configPath.getFileName() + ")";
    }

    @Override
    public boolean supportsExperiments() {
        return false;
    }

    @Override
    public CompletableFuture<Void> close() {
        if (watchService != null) {
            try {
                watchService.close();
            } catch (IOException e) {
                // Log but don't fail
            }
        }
        if (watchExecutor != null) {
            watchExecutor.shutdown();
        }
        return CompletableFuture.completedFuture(null);
    }
}
```

## Key Java-Specific Considerations

1. **Concurrency**: Use `AtomicReference` for thread-safe caching, `CompletableFuture` for async operations
2. **Resource Management**: Implement `AutoCloseable` where appropriate, use try-with-resources
3. **File Watching**: Use `WatchService` API (available since Java 7)
4. **TOML Parsing**: Use jackson-dataformat-toml or toml4j
5. **Expression Parsing**: Implement custom parser or use JEXL/MVEL for expression evaluation
6. **Testing**: Use JUnit 5, Mockito for mocking, Awaitility for async testing

## Dependencies (Maven)

```xml
<dependencies>
    <!-- OpenFeature SDK -->
    <dependency>
        <groupId>dev.openfeature</groupId>
        <artifactId>sdk</artifactId>
        <version>1.7.0</version>
    </dependency>

    <!-- HTTP Client -->
    <dependency>
        <groupId>com.squareup.okhttp3</groupId>
        <artifactId>okhttp</artifactId>
        <version>4.12.0</version>
    </dependency>

    <!-- JSON Processing -->
    <dependency>
        <groupId>com.fasterxml.jackson.core</groupId>
        <artifactId>jackson-databind</artifactId>
        <version>2.16.0</version>
    </dependency>

    <!-- TOML Parsing -->
    <dependency>
        <groupId>com.fasterxml.jackson.dataformat</groupId>
        <artifactId>jackson-dataformat-toml</artifactId>
        <version>2.16.0</version>
    </dependency>

    <!-- Logging -->
    <dependency>
        <groupId>org.slf4j</groupId>
        <artifactId>slf4j-api</artifactId>
        <version>2.0.9</version>
    </dependency>

    <!-- Testing -->
    <dependency>
        <groupId>org.junit.jupiter</groupId>
        <artifactId>junit-jupiter</artifactId>
        <version>5.10.1</version>
        <scope>test</scope>
    </dependency>
    <dependency>
        <groupId>org.mockito</groupId>
        <artifactId>mockito-core</artifactId>
        <version>5.8.0</version>
        <scope>test</scope>
    </dependency>
</dependencies>
```

---

# JavaScript/TypeScript Implementation Plan

## Technology Stack

- **Language**: TypeScript 5.0+
- **Runtime**: Node.js 18+ LTS
- **HTTP Client**: axios or node-fetch
- **File Watching**: chokidar
- **TOML Parser**: @iarna/toml or toml
- **OpenFeature**: @openfeature/server-sdk
- **Build Tool**: npm/yarn/pnpm
- **Module System**: ESM

## Module Structure

```
superposition-provider/
├── package.json
├── tsconfig.json
├── README.md
├── src/
│   ├── interfaces/
│   │   ├── AllFeatureProvider.ts
│   │   ├── FeatureExperimentMeta.ts
│   │   └── SuperpositionDataSource.ts
│   ├── models/
│   │   ├── AllFeatureProviderMetadata.ts
│   │   ├── ExperimentMeta.ts
│   │   ├── ConfigData.ts
│   │   └── ExperimentData.ts
│   ├── datasources/
│   │   ├── HttpDataSource.ts
│   │   └── FileDataSource.ts
│   ├── providers/
│   │   ├── LocalResolutionProvider.ts
│   │   └── LocalResolutionProviderOptions.ts
│   ├── types/
│   │   ├── RefreshStrategy.ts
│   │   └── index.ts
│   ├── utils/
│   │   ├── cacTomlParser.ts
│   │   └── expressionParser.ts
│   └── index.ts
├── test/
│   ├── LocalResolutionProvider.test.ts
│   ├── HttpDataSource.test.ts
│   └── FileDataSource.test.ts
├── test-data/
│   └── example.cac.toml
└── examples/
    ├── localHttp.ts
    ├── localFile.ts
    ├── localFileWatch.ts
    └── allFeatures.ts
```

## Core Interfaces

### AllFeatureProvider Interface

```typescript
// src/interfaces/AllFeatureProvider.ts

import { AllFeatureProviderMetadata } from '../models/AllFeatureProviderMetadata';

/**
 * Interface for bulk configuration resolution
 */
export interface AllFeatureProvider {
  /**
   * Resolve all features for the given evaluation context
   */
  resolveAllFeatures(context: Record<string, any>): Promise<Record<string, any>>;

  /**
   * Resolve all features matching the given prefix filters
   *
   * @param context Evaluation context
   * @param prefixFilter Array of prefixes to filter by (undefined for no filtering)
   */
  resolveAllFeaturesWithFilter(
    context: Record<string, any>,
    prefixFilter?: string[]
  ): Promise<Record<string, any>>;

  /**
   * Get metadata about this provider
   */
  getMetadata(): AllFeatureProviderMetadata;
}
```

### FeatureExperimentMeta Interface

```typescript
// src/interfaces/FeatureExperimentMeta.ts

import { ExperimentMeta } from '../models/ExperimentMeta';

/**
 * Interface for experiment metadata and variant resolution
 */
export interface FeatureExperimentMeta {
  /**
   * Get all applicable variant IDs for the given context
   */
  getApplicableVariants(context: Record<string, any>): Promise<string[]>;

  /**
   * Get detailed experiment metadata for the given context
   */
  getExperimentMetadata(context: Record<string, any>): Promise<ExperimentMeta[]>;

  /**
   * Get the variant for a specific experiment
   *
   * @returns Variant ID or undefined if not applicable
   */
  getExperimentVariant(
    experimentId: string,
    context: Record<string, any>
  ): Promise<string | undefined>;
}
```

### SuperpositionDataSource Interface

```typescript
// src/interfaces/SuperpositionDataSource.ts

import { ConfigData } from '../models/ConfigData';
import { ExperimentData } from '../models/ExperimentData';

/**
 * Interface for abstracting data sources
 */
export interface SuperpositionDataSource {
  /**
   * Fetch the latest configuration from the data source
   */
  fetchConfig(): Promise<ConfigData>;

  /**
   * Fetch experiment data from the data source
   *
   * @returns Experiment data or undefined if not supported
   */
  fetchExperiments(): Promise<ExperimentData | undefined>;

  /**
   * Get a human-readable name for this data source
   */
  getSourceName(): string;

  /**
   * Check if this data source supports experiments
   */
  supportsExperiments(): boolean;

  /**
   * Close and cleanup resources used by this data source
   */
  close(): Promise<void>;
}
```

## Implementation Details

### LocalResolutionProvider

```typescript
// src/providers/LocalResolutionProvider.ts

import { AllFeatureProvider } from '../interfaces/AllFeatureProvider';
import { FeatureExperimentMeta } from '../interfaces/FeatureExperimentMeta';
import { SuperpositionDataSource } from '../interfaces/SuperpositionDataSource';
import { AllFeatureProviderMetadata } from '../models/AllFeatureProviderMetadata';
import { ExperimentMeta } from '../models/ExperimentMeta';
import { LocalResolutionProviderOptions } from './LocalResolutionProviderOptions';
import { Config, Experiments, ExperimentGroups } from '../types';
import { evalConfig, getApplicableVariants } from '../core'; // Core evaluation logic

export class LocalResolutionProvider
  implements AllFeatureProvider, FeatureExperimentMeta {

  private metadata: AllFeatureProviderMetadata;
  private dataSource: SuperpositionDataSource;
  private options: LocalResolutionProviderOptions;

  // Caches
  private cachedConfig: Config | null = null;
  private cachedExperiments: Experiments | null = null;
  private cachedExperimentGroups: ExperimentGroups | null = null;
  private lastConfigUpdate: Date | null = null;
  private lastExperimentsUpdate: Date | null = null;

  // Polling
  private pollingInterval: NodeJS.Timeout | null = null;

  constructor(
    dataSource: SuperpositionDataSource,
    options: LocalResolutionProviderOptions
  ) {
    this.dataSource = dataSource;
    this.options = options;
    this.metadata = new AllFeatureProviderMetadata(
      'LocalResolutionProvider',
      '1.0.0' // Get from package.json
    );
  }

  /**
   * Initialize the provider and start background tasks if needed
   */
  async init(): Promise<void> {
    // Initial fetch
    await this.refreshConfig();

    if (this.options.enableExperiments && this.dataSource.supportsExperiments()) {
      await this.refreshExperiments();
    }

    // Start polling if configured
    if (this.options.refreshStrategy.type === 'polling') {
      this.startPolling();
    }
  }

  private startPolling(): void {
    const interval = this.options.refreshStrategy.interval * 1000;

    this.pollingInterval = setInterval(async () => {
      try {
        await this.refreshConfig();

        if (this.options.enableExperiments && this.dataSource.supportsExperiments()) {
          await this.refreshExperiments();
        }
      } catch (error) {
        console.error('Polling error:', error);
      }
    }, interval);
  }

  private async refreshConfig(): Promise<void> {
    const configData = await this.dataSource.fetchConfig();
    this.cachedConfig = configData.config;
    this.lastConfigUpdate = configData.fetchedAt;
  }

  private async refreshExperiments(): Promise<void> {
    const expData = await this.dataSource.fetchExperiments();
    if (expData) {
      this.cachedExperiments = expData.experiments;
      this.cachedExperimentGroups = expData.experimentGroups;
      this.lastExperimentsUpdate = expData.fetchedAt;
    }
  }

  private async checkAndRefreshIfNeeded(): Promise<void> {
    if (this.options.refreshStrategy.type === 'onDemand') {
      const ttl = this.options.refreshStrategy.ttl;
      const now = new Date();

      if (!this.lastConfigUpdate ||
          (now.getTime() - this.lastConfigUpdate.getTime()) / 1000 > ttl) {
        await this.refreshConfig();
      }

      if (this.options.enableExperiments &&
          this.dataSource.supportsExperiments() &&
          (!this.lastExperimentsUpdate ||
           (now.getTime() - this.lastExperimentsUpdate.getTime()) / 1000 > ttl)) {
        await this.refreshExperiments();
      }
    }
  }

  async resolveAllFeatures(context: Record<string, any>): Promise<Record<string, any>> {
    await this.checkAndRefreshIfNeeded();

    if (!this.cachedConfig) {
      return this.options.fallbackConfig || {};
    }

    // Add variant IDs to context if experiments enabled
    const enhancedContext = { ...context };
    if (this.options.enableExperiments) {
      const variants = await this.getApplicableVariants(context);
      enhancedContext.variantIds = variants;
    }

    // Call core evaluation logic
    return evalConfig(this.cachedConfig, enhancedContext);
  }

  async resolveAllFeaturesWithFilter(
    context: Record<string, any>,
    prefixFilter?: string[]
  ): Promise<Record<string, any>> {
    const allFeatures = await this.resolveAllFeatures(context);

    if (!prefixFilter || prefixFilter.length === 0) {
      return allFeatures;
    }

    const filtered: Record<string, any> = {};
    for (const [key, value] of Object.entries(allFeatures)) {
      if (prefixFilter.some(prefix => key.startsWith(prefix))) {
        filtered[key] = value;
      }
    }

    return filtered;
  }

  getMetadata(): AllFeatureProviderMetadata {
    return this.metadata;
  }

  async getApplicableVariants(context: Record<string, any>): Promise<string[]> {
    if (!this.cachedExperiments || !this.cachedExperimentGroups) {
      return [];
    }

    return getApplicableVariants(
      context,
      this.cachedExperiments,
      this.cachedExperimentGroups
    );
  }

  async getExperimentMetadata(context: Record<string, any>): Promise<ExperimentMeta[]> {
    const variants = await this.getApplicableVariants(context);
    // Build ExperimentMeta from variants and experiments
    // Implementation details...
    return [];
  }

  async getExperimentVariant(
    experimentId: string,
    context: Record<string, any>
  ): Promise<string | undefined> {
    // Implementation details...
    return undefined;
  }

  /**
   * Shutdown the provider and cleanup resources
   */
  async shutdown(): Promise<void> {
    if (this.pollingInterval) {
      clearInterval(this.pollingInterval);
      this.pollingInterval = null;
    }
    await this.dataSource.close();
  }
}
```

### FileDataSource with Chokidar

```typescript
// src/datasources/FileDataSource.ts

import { SuperpositionDataSource } from '../interfaces/SuperpositionDataSource';
import { ConfigData } from '../models/ConfigData';
import { ExperimentData } from '../models/ExperimentData';
import { Config } from '../types';
import * as fs from 'fs/promises';
import * as path from 'path';
import chokidar from 'chokidar';
import { parseCacToml } from '../utils/cacTomlParser';

export interface FileDataSourceOptions {
  configPath: string;
  watchFiles: boolean;
}

export class FileDataSource implements SuperpositionDataSource {
  private configPath: string;
  private watchFiles: boolean;
  private cachedConfig: ConfigData | null = null;
  private watcher: chokidar.FSWatcher | null = null;

  constructor(options: FileDataSourceOptions) {
    this.configPath = options.configPath;
    this.watchFiles = options.watchFiles;
  }

  async init(): Promise<void> {
    // Load initial config
    await this.loadConfig();

    // Start watching if enabled
    if (this.watchFiles) {
      this.startWatching();
    }
  }

  private startWatching(): void {
    this.watcher = chokidar.watch(this.configPath, {
      persistent: true,
      ignoreInitial: true
    });

    this.watcher.on('change', async (path) => {
      console.log(`Config file changed: ${path}, reloading...`);
      try {
        await this.loadConfig();
      } catch (error) {
        console.error('Failed to reload config:', error);
      }
    });

    this.watcher.on('error', (error) => {
      console.error('Watcher error:', error);
    });
  }

  private async loadConfig(): Promise<void> {
    const content = await fs.readFile(this.configPath, 'utf-8');
    const config = parseCacToml(content);
    this.cachedConfig = {
      config,
      fetchedAt: new Date()
    };
  }

  async fetchConfig(): Promise<ConfigData> {
    if (!this.cachedConfig) {
      throw new Error('Config not loaded');
    }
    return this.cachedConfig;
  }

  async fetchExperiments(): Promise<ExperimentData | undefined> {
    // File source doesn't support experiments
    return undefined;
  }

  getSourceName(): string {
    return `FileDataSource(${path.basename(this.configPath)})`;
  }

  supportsExperiments(): boolean {
    return false;
  }

  async close(): Promise<void> {
    if (this.watcher) {
      await this.watcher.close();
      this.watcher = null;
    }
  }
}
```

## Key TypeScript-Specific Considerations

1. **Type Safety**: Full TypeScript types for all interfaces, strong typing for config objects
2. **Async/Await**: Native async/await support throughout
3. **File Watching**: Use chokidar for cross-platform file watching
4. **TOML Parsing**: Use @iarna/toml (pure JS, well-maintained)
5. **Module System**: Use ESM (import/export) for modern Node.js
6. **Testing**: Use Jest or Vitest with TypeScript support
7. **OpenFeature Integration**: Use @openfeature/server-sdk

## Dependencies (package.json)

```json
{
  "name": "@juspay/superposition-provider",
  "version": "1.0.0",
  "type": "module",
  "main": "./dist/index.js",
  "types": "./dist/index.d.ts",
  "dependencies": {
    "@openfeature/server-sdk": "^1.13.0",
    "axios": "^1.6.0",
    "chokidar": "^3.5.3",
    "@iarna/toml": "^2.2.5"
  },
  "devDependencies": {
    "typescript": "^5.3.0",
    "vitest": "^1.0.0",
    "@types/node": "^20.10.0"
  },
  "scripts": {
    "build": "tsc",
    "test": "vitest",
    "example:http": "tsx examples/localHttp.ts",
    "example:file": "tsx examples/localFile.ts",
    "example:watch": "tsx examples/localFileWatch.ts"
  }
}
```

---

# Python Implementation Plan

## Technology Stack

- **Language**: Python 3.10+
- **Async Framework**: asyncio (built-in)
- **HTTP Client**: httpx or aiohttp
- **File Watching**: watchdog
- **TOML Parser**: tomli/tomllib (built-in from 3.11) or toml
- **Type Hints**: Full type annotations with mypy
- **OpenFeature**: openfeature-sdk
- **Package Manager**: pip/poetry/pdm

## Module Structure

```
superposition_provider/
├── pyproject.toml
├── README.md
├── src/
│   └── superposition_provider/
│       ├── __init__.py
│       ├── interfaces/
│       │   ├── __init__.py
│       │   ├── all_feature_provider.py
│       │   ├── feature_experiment_meta.py
│       │   └── superposition_data_source.py
│       ├── models/
│       │   ├── __init__.py
│       │   ├── metadata.py
│       │   ├── experiment_meta.py
│       │   ├── config_data.py
│       │   └── experiment_data.py
│       ├── datasources/
│       │   ├── __init__.py
│       │   ├── http_data_source.py
│       │   └── file_data_source.py
│       ├── providers/
│       │   ├── __init__.py
│       │   ├── local_resolution_provider.py
│       │   └── options.py
│       ├── types/
│       │   ├── __init__.py
│       │   └── refresh_strategy.py
│       ├── utils/
│       │   ├── __init__.py
│       │   ├── cac_toml_parser.py
│       │   └── expression_parser.py
│       └── py.typed
├── tests/
│   ├── __init__.py
│   ├── test_local_resolution_provider.py
│   ├── test_http_data_source.py
│   └── test_file_data_source.py
├── test_data/
│   └── example.cac.toml
└── examples/
    ├── local_http.py
    ├── local_file.py
    ├── local_file_watch.py
    └── all_features.py
```

## Core Interfaces

### AllFeatureProvider Protocol

```python
# src/superposition_provider/interfaces/all_feature_provider.py

from typing import Protocol, Dict, Any, Optional, List
from ..models.metadata import AllFeatureProviderMetadata

class AllFeatureProvider(Protocol):
    """Interface for bulk configuration resolution"""

    async def resolve_all_features(
        self,
        context: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        Resolve all features for the given evaluation context

        Args:
            context: Evaluation context

        Returns:
            Map of feature keys to values
        """
        ...

    async def resolve_all_features_with_filter(
        self,
        context: Dict[str, Any],
        prefix_filter: Optional[List[str]] = None
    ) -> Dict[str, Any]:
        """
        Resolve all features matching the given prefix filters

        Args:
            context: Evaluation context
            prefix_filter: List of prefixes to filter by (None for no filtering)

        Returns:
            Filtered map of features
        """
        ...

    def get_metadata(self) -> AllFeatureProviderMetadata:
        """Get metadata about this provider"""
        ...
```

### FeatureExperimentMeta Protocol

```python
# src/superposition_provider/interfaces/feature_experiment_meta.py

from typing import Protocol, Dict, Any, List, Optional
from ..models.experiment_meta import ExperimentMeta

class FeatureExperimentMeta(Protocol):
    """Interface for experiment metadata and variant resolution"""

    async def get_applicable_variants(
        self,
        context: Dict[str, Any]
    ) -> List[str]:
        """Get all applicable variant IDs for the given context"""
        ...

    async def get_experiment_metadata(
        self,
        context: Dict[str, Any]
    ) -> List[ExperimentMeta]:
        """Get detailed experiment metadata for the given context"""
        ...

    async def get_experiment_variant(
        self,
        experiment_id: str,
        context: Dict[str, Any]
    ) -> Optional[str]:
        """
        Get the variant for a specific experiment

        Returns:
            Variant ID or None if not applicable
        """
        ...
```

### SuperpositionDataSource Protocol

```python
# src/superposition_provider/interfaces/superposition_data_source.py

from typing import Protocol, Optional
from ..models.config_data import ConfigData
from ..models.experiment_data import ExperimentData

class SuperpositionDataSource(Protocol):
    """Interface for abstracting data sources"""

    async def fetch_config(self) -> ConfigData:
        """Fetch the latest configuration from the data source"""
        ...

    async def fetch_experiments(self) -> Optional[ExperimentData]:
        """
        Fetch experiment data from the data source

        Returns:
            Experiment data or None if not supported
        """
        ...

    def get_source_name(self) -> str:
        """Get a human-readable name for this data source"""
        ...

    def supports_experiments(self) -> bool:
        """Check if this data source supports experiments"""
        ...

    async def close(self) -> None:
        """Close and cleanup resources used by this data source"""
        ...
```

## Implementation Details

### LocalResolutionProvider

```python
# src/superposition_provider/providers/local_resolution_provider.py

import asyncio
from datetime import datetime, timedelta
from typing import Dict, Any, Optional, List
from ..interfaces.all_feature_provider import AllFeatureProvider
from ..interfaces.feature_experiment_meta import FeatureExperimentMeta
from ..interfaces.superposition_data_source import SuperpositionDataSource
from ..models.metadata import AllFeatureProviderMetadata
from ..models.experiment_meta import ExperimentMeta
from .options import LocalResolutionProviderOptions
from ..core import eval_config, get_applicable_variants  # Core evaluation logic

class LocalResolutionProvider:
    """
    Provider that performs configuration resolution locally using core evaluation

    This provider fetches configuration from a data source (HTTP, File, etc.)
    and performs resolution locally using the core evaluation engine.
    """

    def __init__(
        self,
        data_source: SuperpositionDataSource,
        options: LocalResolutionProviderOptions
    ):
        self.data_source = data_source
        self.options = options
        self.metadata = AllFeatureProviderMetadata(
            name="LocalResolutionProvider",
            version="1.0.0"  # Get from package metadata
        )

        # Caches
        self._cached_config: Optional[Dict[str, Any]] = None
        self._cached_experiments: Optional[Dict[str, Any]] = None
        self._cached_experiment_groups: Optional[Dict[str, Any]] = None
        self._last_config_update: Optional[datetime] = None
        self._last_experiments_update: Optional[datetime] = None

        # Polling task
        self._polling_task: Optional[asyncio.Task] = None
        self._shutdown = False

    async def init(self) -> None:
        """Initialize the provider and start background tasks if needed"""
        # Initial fetch
        await self._refresh_config()

        if self.options.enable_experiments and self.data_source.supports_experiments():
            await self._refresh_experiments()

        # Start polling if configured
        if self.options.refresh_strategy.type == "polling":
            self._polling_task = asyncio.create_task(self._polling_loop())

    async def _polling_loop(self) -> None:
        """Background task for polling refresh strategy"""
        interval = self.options.refresh_strategy.interval

        while not self._shutdown:
            try:
                await asyncio.sleep(interval)

                await self._refresh_config()

                if self.options.enable_experiments and self.data_source.supports_experiments():
                    await self._refresh_experiments()
            except asyncio.CancelledError:
                break
            except Exception as e:
                print(f"Polling error: {e}")

    async def _refresh_config(self) -> None:
        """Refresh configuration from data source"""
        config_data = await self.data_source.fetch_config()
        self._cached_config = config_data.config
        self._last_config_update = config_data.fetched_at

    async def _refresh_experiments(self) -> None:
        """Refresh experiments from data source"""
        exp_data = await self.data_source.fetch_experiments()
        if exp_data:
            self._cached_experiments = exp_data.experiments
            self._cached_experiment_groups = exp_data.experiment_groups
            self._last_experiments_update = exp_data.fetched_at

    async def _check_and_refresh_if_needed(self) -> None:
        """Check TTL and refresh if needed (for OnDemand strategy)"""
        if self.options.refresh_strategy.type == "on_demand":
            ttl = self.options.refresh_strategy.ttl
            now = datetime.utcnow()

            if (not self._last_config_update or
                (now - self._last_config_update).total_seconds() > ttl):
                await self._refresh_config()

            if (self.options.enable_experiments and
                self.data_source.supports_experiments() and
                (not self._last_experiments_update or
                 (now - self._last_experiments_update).total_seconds() > ttl)):
                await self._refresh_experiments()

    async def resolve_all_features(
        self,
        context: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Resolve all features for the given evaluation context"""
        await self._check_and_refresh_if_needed()

        if not self._cached_config:
            return self.options.fallback_config or {}

        # Add variant IDs to context if experiments enabled
        enhanced_context = context.copy()
        if self.options.enable_experiments:
            variants = await self.get_applicable_variants(context)
            enhanced_context["variantIds"] = variants

        # Call core evaluation logic
        return eval_config(self._cached_config, enhanced_context)

    async def resolve_all_features_with_filter(
        self,
        context: Dict[str, Any],
        prefix_filter: Optional[List[str]] = None
    ) -> Dict[str, Any]:
        """Resolve all features matching the given prefix filters"""
        all_features = await self.resolve_all_features(context)

        if not prefix_filter:
            return all_features

        filtered = {}
        for key, value in all_features.items():
            if any(key.startswith(prefix) for prefix in prefix_filter):
                filtered[key] = value

        return filtered

    def get_metadata(self) -> AllFeatureProviderMetadata:
        """Get metadata about this provider"""
        return self.metadata

    async def get_applicable_variants(
        self,
        context: Dict[str, Any]
    ) -> List[str]:
        """Get all applicable variant IDs for the given context"""
        if not self._cached_experiments or not self._cached_experiment_groups:
            return []

        return get_applicable_variants(
            context,
            self._cached_experiments,
            self._cached_experiment_groups
        )

    async def get_experiment_metadata(
        self,
        context: Dict[str, Any]
    ) -> List[ExperimentMeta]:
        """Get detailed experiment metadata for the given context"""
        # Implementation details...
        return []

    async def get_experiment_variant(
        self,
        experiment_id: str,
        context: Dict[str, Any]
    ) -> Optional[str]:
        """Get the variant for a specific experiment"""
        # Implementation details...
        return None

    async def shutdown(self) -> None:
        """Shutdown the provider and cleanup resources"""
        self._shutdown = True

        if self._polling_task:
            self._polling_task.cancel()
            try:
                await self._polling_task
            except asyncio.CancelledError:
                pass

        await self.data_source.close()
```

### FileDataSource with Watchdog

```python
# src/superposition_provider/datasources/file_data_source.py

import asyncio
from datetime import datetime
from pathlib import Path
from typing import Optional
from dataclasses import dataclass
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler, FileModifiedEvent, FileCreatedEvent

from ..interfaces.superposition_data_source import SuperpositionDataSource
from ..models.config_data import ConfigData
from ..models.experiment_data import ExperimentData
from ..utils.cac_toml_parser import parse_cac_toml

@dataclass
class FileDataSourceOptions:
    config_path: Path
    watch_files: bool = False

class ConfigFileHandler(FileSystemEventHandler):
    """Handler for file system events"""

    def __init__(self, config_path: Path, on_change_callback):
        self.config_path = config_path
        self.on_change_callback = on_change_callback

    def on_modified(self, event):
        if not event.is_directory and Path(event.src_path) == self.config_path:
            print(f"Config file changed: {event.src_path}, reloading...")
            asyncio.create_task(self.on_change_callback())

    def on_created(self, event):
        if not event.is_directory and Path(event.src_path) == self.config_path:
            print(f"Config file created: {event.src_path}, loading...")
            asyncio.create_task(self.on_change_callback())

class FileDataSource:
    """File-based data source with optional file watching"""

    def __init__(self, options: FileDataSourceOptions):
        self.config_path = options.config_path
        self.watch_files = options.watch_files
        self._cached_config: Optional[ConfigData] = None
        self._observer: Optional[Observer] = None

    async def init(self) -> None:
        """Initialize the data source"""
        # Load initial config
        await self._load_config()

        # Start watching if enabled
        if self.watch_files:
            self._start_watching()

    def _start_watching(self) -> None:
        """Start watching config file for changes"""
        event_handler = ConfigFileHandler(
            self.config_path,
            self._load_config
        )

        self._observer = Observer()
        self._observer.schedule(
            event_handler,
            str(self.config_path.parent),
            recursive=False
        )
        self._observer.start()

    async def _load_config(self) -> None:
        """Load config from file"""
        try:
            content = self.config_path.read_text()
            config = parse_cac_toml(content)
            self._cached_config = ConfigData(
                config=config,
                fetched_at=datetime.utcnow()
            )
        except Exception as e:
            print(f"Failed to load config: {e}")
            raise

    async def fetch_config(self) -> ConfigData:
        """Fetch the latest configuration from the data source"""
        if not self._cached_config:
            raise ValueError("Config not loaded")
        return self._cached_config

    async def fetch_experiments(self) -> Optional[ExperimentData]:
        """Fetch experiment data (not supported for file source)"""
        return None

    def get_source_name(self) -> str:
        """Get a human-readable name for this data source"""
        return f"FileDataSource({self.config_path.name})"

    def supports_experiments(self) -> bool:
        """Check if this data source supports experiments"""
        return False

    async def close(self) -> None:
        """Close and cleanup resources"""
        if self._observer:
            self._observer.stop()
            self._observer.join()
            self._observer = None
```

## Key Python-Specific Considerations

1. **Type Hints**: Use Protocol for structural subtyping (duck typing with types)
2. **Async/Await**: Native asyncio support throughout
3. **File Watching**: Use watchdog library (cross-platform)
4. **TOML Parsing**: Use tomllib (Python 3.11+) or tomli for older versions
5. **Dataclasses**: Use dataclasses or Pydantic for models
6. **Context Managers**: Implement `__aenter__`/`__aexit__` for resource management
7. **Testing**: Use pytest with pytest-asyncio
8. **Package Management**: Use poetry or pdm for modern dependency management

## Dependencies (pyproject.toml)

```toml
[project]
name = "superposition-provider"
version = "1.0.0"
description = "Superposition provider library for Python"
requires-python = ">=3.10"
dependencies = [
    "httpx>=0.25.0",
    "watchdog>=3.0.0",
    "tomli>=2.0.1; python_version<'3.11'",
    "openfeature-sdk>=0.5.0",
]

[project.optional-dependencies]
dev = [
    "pytest>=7.4.0",
    "pytest-asyncio>=0.21.0",
    "mypy>=1.7.0",
    "ruff>=0.1.0",
]

[build-system]
requires = ["setuptools>=68.0"]
build-backend = "setuptools.build_meta"

[tool.mypy]
python_version = "3.10"
strict = true
warn_return_any = true
warn_unused_configs = true

[tool.pytest.ini_options]
asyncio_mode = "auto"
testpaths = ["tests"]
```

---

# Cross-Language Compatibility Matrix

| Feature | Rust | Java | JavaScript/TS | Python |
|---------|------|------|---------------|--------|
| **AllFeatureProvider** | Trait | Interface | Interface | Protocol |
| **FeatureExperimentMeta** | Trait | Interface | Interface | Protocol |
| **SuperpositionDataSource** | Trait | Interface | Interface | Protocol |
| **LocalResolutionProvider** | Struct | Class | Class | Class |
| **HTTP Data Source** | ✅ | ✅ | ✅ | ✅ |
| **File Data Source** | ✅ | ✅ | ✅ | ✅ |
| **File Watching** | notify | WatchService | chokidar | watchdog |
| **TOML Parsing** | cac_toml | jackson-toml | @iarna/toml | tomllib |
| **Async Support** | async/await | CompletableFuture | async/await | asyncio |
| **Thread Safety** | Arc<RwLock> | AtomicReference | N/A (single-threaded) | asyncio locks |
| **Polling Refresh** | tokio::spawn | ScheduledExecutor | setInterval | asyncio.Task |
| **OnDemand Refresh** | TTL check | TTL check | TTL check | TTL check |
| **OpenFeature** | ✅ | ✅ | ✅ | ✅ |
| **Experiments** | ✅ | ✅ | ✅ | ✅ |

# Unified API Examples

## Creating a Provider with HTTP Data Source

### Rust
```rust
let data_source = Arc::new(HttpDataSource::new(options));
let provider = Arc::new(LocalResolutionProvider::new(
    data_source,
    LocalResolutionProviderOptions::default()
));
provider.init().await?;
```

### Java
```java
SuperpositionDataSource dataSource = new HttpDataSource(options);
LocalResolutionProvider provider = new LocalResolutionProvider(
    dataSource,
    LocalResolutionProviderOptions.builder().build()
);
provider.init().get();
```

### JavaScript/TypeScript
```typescript
const dataSource = new HttpDataSource(options);
const provider = new LocalResolutionProvider(
    dataSource,
    new LocalResolutionProviderOptions()
);
await provider.init();
```

### Python
```python
data_source = HttpDataSource(options)
provider = LocalResolutionProvider(
    data_source,
    LocalResolutionProviderOptions()
)
await provider.init()
```

## Creating a Provider with File Data Source

### Rust
```rust
let data_source = Arc::new(FileDataSource::new(FileDataSourceOptions {
    config_path: PathBuf::from("config.cac.toml"),
    watch_files: true,
})?);
let provider = Arc::new(LocalResolutionProvider::new(data_source, options));
provider.init().await?;
```

### Java
```java
FileDataSource dataSource = new FileDataSource(
    Paths.get("config.cac.toml"),
    true  // watch files
);
LocalResolutionProvider provider = new LocalResolutionProvider(
    dataSource, options
);
provider.init().get();
```

### JavaScript/TypeScript
```typescript
const dataSource = new FileDataSource({
    configPath: 'config.cac.toml',
    watchFiles: true
});
await dataSource.init();
const provider = new LocalResolutionProvider(dataSource, options);
await provider.init();
```

### Python
```python
data_source = FileDataSource(FileDataSourceOptions(
    config_path=Path('config.cac.toml'),
    watch_files=True
))
await data_source.init()
provider = LocalResolutionProvider(data_source, options)
await provider.init()
```

## Resolving All Features

### All Languages (Unified API)
```
context = {
    "country": "US",
    "platform": "web",
    "user_tier": "premium"
}

features = await provider.resolve_all_features(context)
```

## Resolving with Prefix Filter

### All Languages (Unified API)
```
features = await provider.resolve_all_features_with_filter(
    context,
    ["feature_", "experiment_"]
)
```

# Implementation Phases (Per Language)

## Phase 1: Core Interfaces and Models (Week 1)
- Define all interfaces/traits/protocols
- Create model classes (metadata, config data, experiment data)
- Define type system (refresh strategies, options)

## Phase 2: Data Sources (Week 2)
- Implement HttpDataSource
- Implement FileDataSource with file watching
- Implement CAC TOML parser
- Implement expression parser for JSONLogic conversion

## Phase 3: LocalResolutionProvider (Week 3)
- Implement provider class
- Implement caching logic
- Implement refresh strategies (polling, on-demand)
- Integrate with core evaluation logic

## Phase 4: Examples and Documentation (Week 4)
- Create examples for HTTP data source
- Create examples for file data source
- Create examples for file watching
- Create examples for all features API
- Write comprehensive README
- Write API documentation

## Phase 5: Testing and Integration (Week 5)
- Unit tests for all components
- Integration tests
- OpenFeature integration tests
- Performance benchmarks
- Cross-language compatibility tests

# Testing Strategy

## Unit Tests
- Data source implementations
- Expression parser
- CAC TOML parser
- Refresh strategies
- Context conversion utilities

## Integration Tests
- Full resolution flow with HTTP source
- Full resolution flow with file source
- File watching behavior
- Polling refresh
- On-demand refresh
- Experiment variant injection
- OpenFeature provider integration

## Cross-Language Tests
- Same CAC TOML file produces same results
- Same context produces same resolved features
- Same experiments produce same variant selection
- JSON serialization compatibility

# Documentation Requirements

## Each Language Must Include:

1. **README.md**
   - Overview of provider architecture
   - Installation instructions
   - Quick start guide
   - Usage examples
   - API reference links

2. **API Documentation**
   - Generated from code comments (Javadoc, TSDoc, docstrings, rustdoc)
   - All public interfaces documented
   - Examples for each method

3. **Examples**
   - HTTP data source with polling
   - File data source without watching
   - File data source with watching
   - AllFeatureProvider usage
   - FeatureExperimentMeta usage
   - OpenFeature integration

4. **Migration Guide**
   - How to migrate from existing provider
   - Breaking changes (if any)
   - Best practices

# Success Criteria

✅ All languages implement the same three interfaces
✅ All languages support HTTP and File data sources
✅ All languages support file watching
✅ All languages support both refresh strategies (polling, on-demand)
✅ All languages support experiment variant injection
✅ All languages integrate with OpenFeature SDK
✅ Same CAC TOML config produces identical results across languages
✅ All languages have comprehensive tests (>80% coverage)
✅ All languages have complete documentation
✅ All languages have working examples
✅ Performance benchmarks show acceptable overhead (<10ms for typical config)

# Future Enhancements (Out of Scope)

- RemoteResolutionProvider (API-based resolution)
- Redis data source
- Database data source
- Advanced caching strategies (LRU, TTL-based eviction)
- Metrics and telemetry
- Circuit breaker for HTTP sources
- Config validation and schema enforcement
- Hot-reload without downtime

---

**End of Multi-Language Provider Enhancement Plan**
