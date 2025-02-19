import 'dart:ffi' as ffi;
import 'package:dart_cac_client/types/types.dart';
import 'package:ffi/ffi.dart';
import 'dart:io' show Platform;
import 'package:path/path.dart' as path;

// Define the library
final ffi.DynamicLibrary _lib = ffi.DynamicLibrary.open(_libName);

// Helper to get the correct library name based on the platform
String get _libName {
  if (Platform.isWindows) {
    return path.join(
        '/Users/subhash/working-repos/github/superposition/target/debug/',
        'libexperimentation_client.dll');
  }
  if (Platform.isMacOS) {
    return path.join(
        '/Users/subhash/working-repos/github/superposition/target/debug/',
        'libexperimentation_client.dylib');
  }
  return path.join(
      '/Users/subhash/working-repos/github/superposition/target/debug/',
      'libexperimentation_client.so');
}

// Bind the C functions
final ExptNewClientDart _exptNewClient = _lib
    .lookup<ffi.NativeFunction<ExptNewClientNative>>('expt_new_client')
    .asFunction();

final ExptGetClientDart _exptGetClient = _lib
    .lookup<ffi.NativeFunction<ExptGetClientNative>>('expt_get_client')
    .asFunction();

final ExptLastErrorLengthDart _exptLastErrorLength = _lib
    .lookup<ffi.NativeFunction<ExptLastErrorLengthNative>>(
        'expt_last_error_length')
    .asFunction();

final ExptLastErrorMessageDart _exptLastErrorMessage = _lib
    .lookup<ffi.NativeFunction<ExptLastErrorMessageNative>>(
        'expt_last_error_message')
    .asFunction();

final ExptFreeStringDart _exptFreeString = _lib
    .lookup<ffi.NativeFunction<ExptFreeStringNative>>('expt_free_string')
    .asFunction();

final ExpStartPollingUpdateDart _exptStartPollingUpdate = _lib
    .lookup<ffi.NativeFunction<ExpStartPollingUpdateNative>>(
        'expt_start_polling_update')
    .asFunction();

final ExptFreeClientDart _exptFreeClient = _lib
    .lookup<ffi.NativeFunction<ExptFreeClientNative>>('expt_free_client')
    .asFunction();

final ExptGetApplicableVariantDart _exptGetApplicableVariant = _lib
    .lookup<ffi.NativeFunction<ExptGetApplicableVariantNative>>(
        'expt_get_applicable_variant')
    .asFunction();

final ExptGetSatisfiedExperimentsDart _exptGetSatisfiedExperiments = _lib
    .lookup<ffi.NativeFunction<ExptGetSatisfiedExperimentsNative>>(
        'expt_get_satisfied_experiments')
    .asFunction();

final ExptGetFilteredSatisfiedExperimentsDart
    _exptGetFilteredSatisfiedExperiments = _lib
        .lookup<ffi.NativeFunction<ExptGetFilteredSatisfiedExperimentsNative>>(
            'expt_get_filtered_satisfied_experiments')
        .asFunction();

final ExptGetRunningExperimentsDart _exptGetRunningExperiments = _lib
    .lookup<ffi.NativeFunction<ExptGetRunningExperimentsNative>>(
        'expt_get_running_experiments')
    .asFunction();

// Dart wrapper class
class ExptClient {
  late ffi.Pointer<ffi.Void> _clientPtr;

  ExptClient(String tenant, int updateFrequency, String hostname) {
    final tenantPtr = tenant.toNativeUtf8();
    final hostnamePtr = hostname.toNativeUtf8();

    final result = _exptNewClient(tenantPtr, updateFrequency, hostnamePtr);
    print("Expt Client result: $result");

    _exptFreeString(tenantPtr);
    _exptFreeString(hostnamePtr);

    if (result != 0) {
      final errorPtr = _exptLastErrorMessage();
      final errorMessage = errorPtr.toDartString();
      print("Error message: $errorMessage");
      _exptFreeString(errorPtr);
      throw Exception("Failed to create Experimentation client: $errorMessage");
    }

    _clientPtr = _exptGetClient(tenant.toNativeUtf8());
    if (_clientPtr == ffi.nullptr) {
      final errorPtr = _exptLastErrorMessage();
      final errorMessage = errorPtr.toDartString();
      print("Error getting client pointer: $errorMessage");
      _exptFreeString(errorPtr);
      throw Exception("Failed to get Experimentation client: $errorMessage");
    }
    print("Client pointer obtained successfully");
  }

  String getApplicableVariants(String context, int toss) {
    final clientPtr = context.toNativeUtf8();
    final tossPtr = malloc.allocate<ffi.Int16>(ffi.sizeOf<ffi.Int16>());
    tossPtr.value = toss;
    final applicableVariantPtr =
        _exptGetApplicableVariant(_clientPtr, clientPtr, tossPtr);

    _exptFreeString(clientPtr);
    malloc.free(tossPtr);

    if (applicableVariantPtr == ffi.nullptr) {
      final errorPtr = _exptLastErrorMessage();
      final errorMessage = errorPtr.toDartString();
      print("Error getting Applicable Variant: $errorMessage");
      _exptFreeString(errorPtr);
      throw Exception("Failed to get config: $errorMessage");
    }
    final applicableVariant = applicableVariantPtr.toDartString();
    print("Received Applicable Variant: $applicableVariant");
    _exptFreeString(applicableVariantPtr);

    return applicableVariant;
  }

  String getSatisfiedExperiments(String context, String filterPrefix) {
    final contextPtr = context.toNativeUtf8();
    final filterPrefixPtr = filterPrefix.toNativeUtf8();

    final satisfiedExperimentsPtr =
        _exptGetSatisfiedExperiments(_clientPtr, contextPtr, filterPrefixPtr);

    _exptFreeString(contextPtr);
    _exptFreeString(filterPrefixPtr);

    if (satisfiedExperimentsPtr == ffi.nullptr) {
      final errorPtr = _exptLastErrorMessage();
      final errorMessage = errorPtr.toDartString();
      print("Error getting satisfied experiments: $errorMessage");
      _exptFreeString(errorPtr);
      throw Exception("Failed to get satisfied experiments: $errorMessage");
    }

    final satisfiedExperiments = satisfiedExperimentsPtr.toDartString();
    print("Received satisfied experiments: $satisfiedExperiments");
    _exptFreeString(satisfiedExperimentsPtr);

    return satisfiedExperiments;
  }

  String getFilteredSatisfiedExperiments(String context, String filterPrefix) {
    if (_clientPtr == ffi.nullptr) {
      throw Exception("Failed to get Experimentation client!");
    }
    final contextPtr = context.toNativeUtf8();
    final filterPrefixPtr = filterPrefix.toNativeUtf8();

    final fltSatisfiedExperimentsPtr = _exptGetFilteredSatisfiedExperiments(
        _clientPtr, contextPtr, filterPrefixPtr);

    _exptFreeString(contextPtr);
    _exptFreeString(filterPrefixPtr);

    if (fltSatisfiedExperimentsPtr == ffi.nullptr) {
      final errorPtr = _exptLastErrorMessage();
      final errorMessage = errorPtr.toDartString();
      print("Error getting filtered satisfied experiments : $errorMessage");
      _exptFreeString(errorPtr);
      throw Exception(
          "Failed to get filtered satisfied experiments: $errorMessage");
    }
    var fltSatisfiedExperiments = fltSatisfiedExperimentsPtr.toDartString();
    _exptFreeString(fltSatisfiedExperimentsPtr);

    return fltSatisfiedExperiments;
  }

  String getRunningExperiments() {
    if (_clientPtr == ffi.nullptr) {
      throw Exception("Failed to get Experimentation client!");
    }
    final runningExprsPtr = _exptGetRunningExperiments(_clientPtr);
    if (runningExprsPtr == ffi.nullptr) {
      final errorPtr = _exptLastErrorMessage();
      final errorMessage = errorPtr.toDartString();
      print("Error getting filtered running experiments : $errorMessage");
      _exptFreeString(errorPtr);
      throw Exception(
          "Failed to get filtered running experiments: $errorMessage");
    }
    var runningExprs = runningExprsPtr.toDartString();
    _exptFreeString(runningExprsPtr);
    return runningExprs;
  }

  void startPollingUpdate(String tenant) {
    if (_clientPtr == ffi.nullptr) {
      throw Exception("Failed to get Experimentation client!");
    }
    final tenantPtr = tenant.toNativeUtf8();
    _exptStartPollingUpdate(tenantPtr);
    _exptFreeString(tenantPtr);
    print("finish polling");
  }

  void dispose() {
    if (_clientPtr != ffi.nullptr) {
      _exptFreeClient(_clientPtr);
      _clientPtr = ffi.nullptr;
    }
  }
}
