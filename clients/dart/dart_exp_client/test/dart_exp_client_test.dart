import 'package:dart_cac_client/dart_exp_client.dart';
import 'package:flutter_test/flutter_test.dart';

void main() {
  test('adds one to input values', () async {
    final client = DartExptClient('dev', 1, 'http://localhost:8080');

    print("Client created successfully");
    try {
      client.exptStartPolling("dev");
      // Perform various operations
      try {
        var applicableVariants = client.getApplicableVariants(
            "{\"os\": \"android\", \"client\": \"1mg\"}", 10);
        print('Full Applicable variants: $applicableVariants');

        var satisfiedExperiments =
            client.getSatisfiedExperiments("juspay", "key1");
        print("Satisfied Expirments: $satisfiedExperiments");

        var filteredsatisfiedExperiments =
            client.getFilteredSatisfiedExperiments("juspay", "key1");
        print("filtered satisfied experiments: $filteredsatisfiedExperiments");

        var runningExperiments = client.getRunningExperiments();
        print("running experiments: $runningExperiments");
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
