import 'package:flutter_test/flutter_test.dart';

import 'package:dart_cac_client/dart_cac_client.dart';

void main() {
  test('adds one to input values', () {
    final dartClient = DartCacClient();
    dartClient.cacStartPolling("dev");
    dartClient.getConfigs('{"country": "India"}', 'country');
    dartClient.getCacLastModified();
    dartClient.getDefaultConfig("default_configs");
    dartClient.getResolvedConfig('{"query": "example"}', "key1, key2", "MERGE");
  });
}
