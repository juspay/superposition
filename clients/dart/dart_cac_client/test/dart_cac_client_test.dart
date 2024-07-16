import 'package:flutter_test/flutter_test.dart';

import 'package:dart_cac_client/dart_cac_client.dart';

void main() {
  test('adds one to input values', () async {
    final client = DartCacClient('dev', 1, 'http://localhost:8080');
    print("Client created successfully");

    try {
      client.cacStartPolling("dev");
      // Perform various operations
      try {
        var config = client.getConfigs('{"country": "India"}', 'country');
        print('Full Config: $config');

        var lastModified = client.getCacLastModified();
        print("Last Modified: $lastModified");

        var resolvedConfig = client.getResolvedConfig('{"country": "India"}',
            "country_image_url,hyperpay_version", MergeStrategy.REPLACE);
        print("Resolved Config: $resolvedConfig");

        var defaultConfig =
            client.getDefaultConfig("country_image_url,hyperpay_version");
        print("Default Config: $defaultConfig");
      } catch (e) {
        print("Error during operations: $e");
      } finally {
        client.dispose();
      }
    } catch (e) {
      print("Error creating client: $e");
    }
  });
}
