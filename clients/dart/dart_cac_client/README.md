# Dart CAC Client

This package provides a Dart interface for the CAC (Configuration as Code) client, allowing you to interact with the CAC server to retrieve and manage configurations.

## Table of Contents
- [Dart CAC Client](#dart-cac-client)
  - [Table of Contents](#table-of-contents)
  - [Installation](#installation)
  - [Usage](#usage)
    - [Creating a Client](#creating-a-client)
    - [Starting Polling for Updates](#starting-polling-for-updates)
    - [Retrieving Configurations](#retrieving-configurations)
    - [Getting Default Configurations](#getting-default-configurations)
    - [Getting Resolved Configurations](#getting-resolved-configurations)
    - [Getting Last Modified Timestamp](#getting-last-modified-timestamp)
    - [Disposing the Client](#disposing-the-client)

## Installation

Add the following dependency to your `pubspec.yaml` file:

```yaml
dependencies:
  dart_cac_client:
    git:
      url: git@github.com:your_repo/dart_cac_client.git
```

## Usage

### Creating a Client
To create a new CAC client, instantiate the DartCacClient class with the tenant name, update frequency (in seconds), and host URL

```dart
final client = DartCacClient('dev', 60, 'http://localhost:8080');
```

### Starting Polling for Updates
Use the cacStartPolling method to start polling for configuration updates for the specified tenant:

```dart
client.cacStartPolling("<tenant name>");
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
