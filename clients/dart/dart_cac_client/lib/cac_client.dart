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
        'cac_client.dll');
  }
  if (Platform.isMacOS) {
    return path.join(
        '/Users/subhash/working-repos/github/superposition/target/debug/',
        'libcac_client.dylib');
  }
  return path.join(
      '/Users/subhash/working-repos/github/superposition/target/debug/',
      'libcac_client.so');
}

// Bind the C functions
final CacNewClientDart _cacNewClient = _lib
    .lookup<ffi.NativeFunction<CacNewClientNative>>('cac_new_client')
    .asFunction();

final CacGetClientDart _cacGetClient = _lib
    .lookup<ffi.NativeFunction<CacGetClientNative>>('cac_get_client')
    .asFunction();

final CacFreeClientDart _cacFreeClient = _lib
    .lookup<ffi.NativeFunction<CacFreeClientNative>>('cac_free_client')
    .asFunction();

final CacGetConfigDart _cacGetConfig = _lib
    .lookup<ffi.NativeFunction<CacGetConfigNative>>('cac_get_config')
    .asFunction();

final CacGetDefaultConfigDart _cacGetDefaultConfigs = _lib
    .lookup<ffi.NativeFunction<CacGetDefaultConfigNative>>(
        'cac_get_default_config')
    .asFunction();

final CacGetLastModifiedDart _cacGetLastModified = _lib
    .lookup<ffi.NativeFunction<CacGetLastModifiedNative>>(
        'cac_get_last_modified')
    .asFunction();

final CacFreeStringDart _cacFreeString = _lib
    .lookup<ffi.NativeFunction<CacFreeStringNative>>('cac_free_string')
    .asFunction();

final CacLastErrorMessageDart _cacLastErrorMessage = _lib
    .lookup<ffi.NativeFunction<CacLastErrorMessageNative>>(
        'cac_last_error_message')
    .asFunction();

final CacGetResolvedConfigsDart _cacGetResolvedConfigs = _lib
    .lookup<ffi.NativeFunction<CacGetResolvedConfigsNative>>(
        'cac_get_resolved_config')
    .asFunction();

final CacStartPollingUpdateDart _cacStartPollingUpdate = _lib
    .lookup<ffi.NativeFunction<CacStartPollingUpdateNative>>(
        'cac_start_polling_update')
    .asFunction();

// Dart wrapper class
class CacClient {
  late ffi.Pointer<ffi.Void> _clientPtr;

  CacClient(String tenant, int updateFrequency, String hostname) {
    print(
        "Creating client with: Tenant: $tenant, UpdateFrequency: $updateFrequency, Hostname: $hostname");

    final tenantPtr = tenant.toNativeUtf8();
    final hostnamePtr = hostname.toNativeUtf8();

    final result = _cacNewClient(tenantPtr, updateFrequency, hostnamePtr);
    print("cac_new_client result: $result");

    malloc.free(tenantPtr);
    malloc.free(hostnamePtr);

    if (result != 0) {
      final errorPtr = _cacLastErrorMessage();
      final errorMessage = errorPtr.toDartString();
      print("Error message: $errorMessage");
      _cacFreeString(errorPtr);
      throw Exception("Failed to create CAC client: $errorMessage");
    }

    print("Client created successfully, attempting to get client pointer");

    _clientPtr = _cacGetClient(tenant.toNativeUtf8());
    if (_clientPtr == ffi.nullptr) {
      final errorPtr = _cacLastErrorMessage();
      final errorMessage = errorPtr.toDartString();
      print("Error getting client pointer: $errorMessage");
      _cacFreeString(errorPtr);
      throw Exception("Failed to get CAC client: $errorMessage");
    }

    print("Client pointer obtained successfully");
  }

  String getConfig(String filterQuery, String filterPrefix) {
    print(
        "Attempting to get config with filterQuery: $filterQuery, filterPrefix: $filterPrefix");

    final filterQueryPtr = filterQuery.toNativeUtf8();
    final filterPrefixPtr = filterPrefix.toNativeUtf8();

    final configPtr =
        _cacGetConfig(_clientPtr, filterQueryPtr, filterPrefixPtr);

    malloc.free(filterQueryPtr);
    malloc.free(filterPrefixPtr);

    if (configPtr == ffi.nullptr) {
      final errorPtr = _cacLastErrorMessage();
      final errorMessage = errorPtr.toDartString();
      print("Error getting config: $errorMessage");
      _cacFreeString(errorPtr);
      throw Exception("Failed to get config: $errorMessage");
    }

    print("Config pointer received, converting to Dart string");
    final config = configPtr.toDartString();
    print("Received config: $config");
    _cacFreeString(configPtr);

    return config;
  }

  String getDefaultConfigs(String filterKeys) {
    final filterKeysPtr = filterKeys.toNativeUtf8();

    final configPtr = _cacGetDefaultConfigs(_clientPtr, filterKeysPtr);

    malloc.free(filterKeysPtr);

    if (configPtr == ffi.nullptr) {
      final errorPtr = _cacLastErrorMessage();
      final errorMessage = errorPtr.toDartString();
      print("Error getting config: $errorMessage");
      _cacFreeString(errorPtr);
      throw Exception("Failed to get config: $errorMessage");
    }

    final config = configPtr.toDartString();
    print("Received Default config: $config");
    _cacFreeString(configPtr);

    return config;
  }

  String getLastModified() {
    if (_clientPtr == ffi.nullptr) {
      throw Exception("Failed to get CAC client!");
    }
    var lastModified = _cacGetLastModified(_clientPtr);
    return lastModified.toDartString();
  }

  String getResolvedConfigs(
      String query, String filterKeys, String mergeStrategy) {
    if (_clientPtr == ffi.nullptr) {
      throw Exception("Failed to get CAC client!");
    }

    final queryPtr = query.toNativeUtf8();
    final filterKeysPtr = filterKeys.toNativeUtf8();
    final mergeStrategyPtr = mergeStrategy.toNativeUtf8();

    final resolvedConfigPtr = _cacGetResolvedConfigs(
        _clientPtr, queryPtr, filterKeysPtr, mergeStrategyPtr);

    malloc.free(queryPtr);
    malloc.free(filterKeysPtr);
    malloc.free(mergeStrategyPtr);

    if (resolvedConfigPtr == ffi.nullptr) {
      final errorPtr = _cacLastErrorMessage();
      final errorMessage = errorPtr.toDartString();
      print("Error getting config: $errorMessage");
      _cacFreeString(errorPtr);
      throw Exception("Failed to get config: $errorMessage");
    }

    return resolvedConfigPtr.toDartString();
  }

  void startPollingUpdate(String tenant) {
    if (_clientPtr == ffi.nullptr) {
      throw Exception("Failed to get CAC client!");
    }
    final tenantPtr = tenant.toNativeUtf8();
    _cacStartPollingUpdate(tenantPtr);
    return;
  }

  void dispose() {
    if (_clientPtr != ffi.nullptr) {
      _cacFreeClient(_clientPtr);
      _clientPtr = ffi.nullptr;
    }
  }
}
