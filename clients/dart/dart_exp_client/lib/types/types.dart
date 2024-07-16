import 'dart:ffi' as ffi;
import 'package:ffi/ffi.dart';

// Define the C functions
typedef ExptNewClientNative = ffi.Int32 Function(
    ffi.Pointer<Utf8>, ffi.Uint64, ffi.Pointer<Utf8>);
typedef ExptNewClientDart = int Function(
    ffi.Pointer<Utf8>, int, ffi.Pointer<Utf8>);

typedef ExptGetClientNative = ffi.Pointer<ffi.Void> Function(ffi.Pointer<Utf8>);
typedef ExptGetClientDart = ffi.Pointer<ffi.Void> Function(ffi.Pointer<Utf8>);

typedef ExptFreeClientNative = ffi.Void Function(ffi.Pointer<ffi.Void>);
typedef ExptFreeClientDart = void Function(ffi.Pointer<ffi.Void>);

typedef ExptGetApplicableVariantNative = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>, ffi.Pointer<Utf8>, ffi.Pointer<ffi.Int16>);
typedef ExptGetApplicableVariantDart = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>, ffi.Pointer<Utf8>, ffi.Pointer<ffi.Int16>);

typedef ExptGetSatisfiedExperimentsNative = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>, ffi.Pointer<Utf8>, ffi.Pointer<Utf8>);
typedef ExptGetSatisfiedExperimentsDart = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>, ffi.Pointer<Utf8>, ffi.Pointer<Utf8>);

typedef ExptGetFilteredSatisfiedExperimentsNative = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>, ffi.Pointer<Utf8>, ffi.Pointer<Utf8>);
typedef ExptGetFilteredSatisfiedExperimentsDart = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>, ffi.Pointer<Utf8>, ffi.Pointer<Utf8>);

typedef ExptGetRunningExperimentsNative = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>);
typedef ExptGetRunningExperimentsDart = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>);

typedef ExptGetLastModifiedNative = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>);

typedef ExptFreeStringNative = ffi.Void Function(ffi.Pointer<Utf8>);
typedef ExptFreeStringDart = void Function(ffi.Pointer<Utf8>);

typedef ExptLastErrorMessageNative = ffi.Pointer<Utf8> Function();
typedef ExptLastErrorMessageDart = ffi.Pointer<Utf8> Function();

typedef ExptLastErrorLengthNative = ffi.Pointer<Utf8> Function();
typedef ExptLastErrorLengthDart = ffi.Pointer<Utf8> Function();

typedef ExpGetResolvedConfigsNative = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>,
    ffi.Pointer<Utf8>,
    ffi.Pointer<Utf8>,
    ffi.Pointer<Utf8>);
typedef ExpGetResolvedConfigsDart = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>,
    ffi.Pointer<Utf8>,
    ffi.Pointer<Utf8>,
    ffi.Pointer<Utf8>);

typedef ExpStartPollingUpdateNative = ffi.Pointer<ffi.Void> Function(
    ffi.Pointer<Utf8>);
typedef ExpStartPollingUpdateDart = ffi.Pointer<ffi.Void> Function(
    ffi.Pointer<Utf8>);

typedef ExpGetLastModifiedDart = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>);
