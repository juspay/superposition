# Dart Experimentation Client

This package provides a Dart interface for the Experimentation client, allowing you to interact with the experimentation server to retrieve and manage experiments.

## Table of Contents
- [Dart Experimentation Client](#dart-experimentation-client)
  - [Table of Contents](#table-of-contents)
  - [Usage](#usage)
    - [Creating a Client](#creating-a-client)
    - [Starting Polling for Updates](#starting-polling-for-updates)
    - [Retrieving Configurations](#retrieving-configurations)
    - [Getting Default Configurations](#getting-default-configurations)
    - [Getting Resolved Configurations](#getting-resolved-configurations)
    - [Getting Last Modified Timestamp](#getting-last-modified-timestamp)
    - [Disposing the Client](#disposing-the-client)

## Usage

### Creating a Client
To create a new DartExptClient client, instantiate the DartCacClient class with the tenant name, update frequency (in seconds), and host URL

```dart
final client = To create a new DartExptClient client, instantiate the DartCacClient class with the tenant name, update frequency (in seconds), and host URL
('dev', 60, 'http://localhost:8080');
```

### Starting Polling for Updates
Use the exptStartPolling method to start polling for configuration updates for the specified tenant:

```dart
client.exptStartPolling("<tenant name>");
```

### Retrieving Configurations
Use the getConfigs method to retrieve configurations based on a filter query and filter prefix:
```dart
String configs = client.getConfigs('{"country": "India"}', 'country');
```

### Getting Default Configurations
Use the getDefaultConfig method to retrieve the default configurations for the specified keys:

```dart
String defaultConfigs = client.getDefaultConfig("india");
```

### Getting Resolved Configurations
Use the getResolvedConfig method to retrieve resolved configurations based on the provided query, keys, and merge strategy:


```dart
String resolvedConfigs = client.getResolvedConfig(
    '{"query": "example"}', "key1, key2", MergeStrategy.MERGE/MergeStrategy.REPLACE);
```


### Getting Last Modified Timestamp
Use the getCacLastModified method to get the last modified timestamp of the configurations:

```dart
String lastModified = client.getCacLastModified();
```

### Disposing the Client
Ensure that you dispose of the client properly to free up resources:
```dart
client.dispose();
```
