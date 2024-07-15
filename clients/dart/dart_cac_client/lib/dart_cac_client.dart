library dart_cac_client;

import 'package:dart_cac_client/cac_client.dart';

enum MergeStrategy { REPLACE, MERGE }

class DartCacClient {
  late CacClient client;

  /// Creates a new CacClient instance.
  ///
  /// [tenant] is the name of the tenant.
  /// [updateFrequency] is the frequency of updates in seconds.
  /// [hostUrl] is the URL of the CAC server.
  ///
  /// Example:
  /// ```dart
  /// final client = CacClient('dev', 60, 'http://localhost:8080');
  /// ```

  DartCacClient(String s, int i, String t) {
    client = CacClient('dev', 1, 'http://localhost:8080');
  }

  /// Retrieves configurations based on the provided query and filter prefix.
  ///
  /// [filterQuery] is a JSON string representing the query parameters.
  /// [filterPrefix] is a comma-separated string of prefixes to filter the configs.
  ///
  /// Returns a JSON string containing the matched configurations.
  ///
  /// Throws an exception if the retrieval fails.
  ///
  /// Example:
  /// ```dart
  /// var configs = client.getConfig('{"country": "India"}', 'country');
  /// print('Configs: $configs');
  /// ```
  String getConfigs(String filterQuery, String filterPrefix) {
    try {
      var configs = client.getConfig(filterQuery, filterPrefix);
      print('Configs: $configs');
      return configs;
    } catch (e) {
      print("Something went wrong ${e}");
    }
    return "{}";
  }

  /// Retrieves the last modified timestamp of the configurations.
  /// Returns a string representing the last modification time.
  ///
  /// Throws an exception if the retrieval fails.
  ///
  /// Example:
  /// ```dart
  /// var lastModified = client.getLastModified();
  /// print("Last Modified was: $lastModified");
  /// ```

  String getCacLastModified() {
    try {
      var lastModified = client.getLastModified();
      print("Last Modified was: $lastModified");
      return lastModified;
    } catch (e) {
      print("Failed to get last modified value");
    }
    return "{}";
  }

  /// Retrieves the default configurations for the specified keys.
  ///
  /// [filterKeys] is a comma-separated string of configuration keys.
  ///
  /// Returns a JSON string containing the default configurations.
  ///
  /// Throws an exception if the retrieval fails.
  ///
  /// Example:
  /// ```dart
  /// var defaultConfigs = client.getDefaultConfigs("india");
  /// print("Default config is $defaultConfigs");
  /// ```
  String getDefaultConfig(String filterKeys) {
    try {
      var defaultConfigs = client.getDefaultConfigs(filterKeys);
      print("Default config is $defaultConfigs");
      return defaultConfigs;
    } catch (e) {
      print("Something went wrong while getting default configs $e");
    }
    return "{}";
  }

  /// Retrieves resolved configurations based on the provided query, keys, and merge strategy.
  ///
  /// [query] is a JSON string representing the query parameters.
  /// [filterKeys] is a comma-separated string of configuration keys to retrieve.
  /// [mergeStrategy] is either "MERGE" or "REPLACE", determining how configs are combined.
  ///
  /// Returns a JSON string containing the resolved configurations.
  ///
  /// Throws an exception if the retrieval fails.
  ///
  /// Example:
  /// ```dart
  /// var resolvedConfigs = client.getResolvedConfigs(
  ///     '{"query": "example"}', "key1, key2", "MERGE");
  /// print("Resolved Config: $resolvedConfigs");
  /// ```
  String getResolvedConfig(
      String query, String filterKeys, MergeStrategy mergeStrategy) {
    try {
      var resolvedConfigs = "{}";
      switch (mergeStrategy) {
        case MergeStrategy.REPLACE:
          resolvedConfigs =
              client.getResolvedConfigs(query, filterKeys, "REPLACE");
          break;
        case MergeStrategy.MERGE:
          resolvedConfigs =
              client.getResolvedConfigs(query, filterKeys, "MERGE");
          break;
        default:
          throw Exception(
              "Invalid merge strategy! Please pass merge strategy of MergeStrategy");
      }
      print("Resolved Config: $resolvedConfigs");
      return resolvedConfigs;
    } catch (e) {
      print("Something went wrong while getting resolved configs $e");
    }
    return "{}";
  }

  /// Starts polling for configuration updates for the specified tenant.
  ///
  /// [tenant] is the name of the tenant to poll updates for.
  ///
  /// Throws an exception if the polling start fails.
  ///
  /// Example:
  /// ```dart
  /// client.startPollingUpdate("dev");
  /// ```
  void cacStartPolling(String tenant) async {
    try {
      client.startPollingUpdate(tenant);
      print("Started polling update for tenant: $tenant");
    } catch (e) {
      print("Something went wrong while starting polling update: $e");
    }
  }

  void dispose() {
    client.dispose();
  }
}
