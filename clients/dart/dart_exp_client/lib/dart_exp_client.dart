library dart_cac_client;

import 'package:dart_cac_client/client.dart';

enum MergeStrategy { REPLACE, MERGE }

class DartExptClient {
  late ExptClient client;

  /// Creates a new CacClient instance.
  ///
  /// [tenant] is the name of the tenant.
  /// [updateFrequency] is the frequency of updates in seconds.
  /// [hostUrl] is the URL of the CAC server.
  ///
  /// Example:
  /// ```dart
  /// final dartClient = DartExptClient('dev', 60, 'http://localhost:8080');
  /// ```

  DartExptClient(String tenant, int updateFrequency, String hostUrl) {
    client = ExptClient(tenant, updateFrequency, hostUrl);
  }

  /// Start polling the superposition server for updates for the given tenant.
  ///
  /// [tenant] is the name of the tenant to poll updates for.
  ///
  /// Example:
  /// ```dart
  /// client.exptStartPolling("dev");
  /// ```
  void exptStartPolling(String tenant) async {
    try {
      client.startPollingUpdate(tenant);
      print("Started polling update for tenant: $tenant");
    } catch (e) {
      print("Something went wrong while starting polling update: $e");
    }
  }

  /// get the experiments that apply to a given context.
  ///
  /// [context] is a JSON string representing the query parameters.
  /// [toss] a number toss between 0 - 100 that is used to assign a variant IDs.
  ///
  /// Returns a string formatted array of variant IDs that match the parameters passed.
  ///
  /// Throws an exception if the retrieval fails.
  ///
  /// Example:
  /// ```dart
  /// var configs = dartClient.getApplicableVariants("{\"os\": \"android\", \"client\": \"1mg\"}", 10)
  /// ```
  String getApplicableVariants(String context, int toss) {
    try {
      var applicableVariant = client.getApplicableVariants(context, toss);
      return applicableVariant;
    } catch (e) {
      print("Something went wrong ${e}");
    }
    return "[]";
  }

  /// get the experiments that apply to a given context c_context. It also filters on config key prefix.
  ///
  /// [context] string value representing the context.
  /// [filterPrefix] key prefix.
  ///
  /// returns a string formatted array of experiments that match the parameters passed.
  /// Example:
  /// ```dart
  /// var satisfiedExperiments =
  ///     dartClient.getSatisfiedExperiments("{\"os\": \"android\", \"client\": \"1mg\"}", "os");
  /// ```
  String getSatisfiedExperiments(String context, String filterPrefix) {
    try {
      var satisfiedExperiments =
          client.getSatisfiedExperiments(context, filterPrefix);
      return satisfiedExperiments;
    } catch (e) {
      print("Something went wrong while getting satisfied experiments $e");
    }
    return "[]";
  }

  /// gets experiments that apply to a given context c_context. It also filters on config key prefix
  ///
  /// [context] is a string representing the experiment context.
  /// [filterPrefix] is a comma-separated string of configuration keys to retrieve.
  ///
  /// returns a string formatted array of experiments that match the parameters passed.
  ///
  /// Example:
  /// ```dart
  /// var fltrSatisfiedExpt =
  ///   client.getFilteredSatisfiedExperiments("{\"os\": \"android\", \"client\": \"1mg\"}", "client");
  /// ```
  String getFilteredSatisfiedExperiments(String context, String filterPrefix) {
    try {
      var fltrSatisfiedExpt =
          client.getFilteredSatisfiedExperiments(context, filterPrefix);
      return fltrSatisfiedExpt;
    } catch (e) {
      print(
          "Something went wrong while getting filtered satisfied experiments $e");
    }
    return "[]";
  }

  /// get all currently running experiments
  /// returns a string formatted array of experiments that match the parameters passed
  String getRunningExperiments() {
    try {
      return client.getRunningExperiments();
    } catch (e) {
      print("Something went wrong while fetching running experiments $e");
    }
    return "[]";
  }

  void dispose() {
    client.dispose();
  }
}
