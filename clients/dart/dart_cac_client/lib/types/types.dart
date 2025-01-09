import 'dart:ffi' as ffi;
import 'package:ffi/ffi.dart';

// Define the C functions
typedef CacNewClientNative = ffi.Int32 Function(
    ffi.Pointer<Utf8>, ffi.Uint64, ffi.Pointer<Utf8>);
typedef CacNewClientDart = int Function(
    ffi.Pointer<Utf8>, int, ffi.Pointer<Utf8>);

typedef CacGetClientNative = ffi.Pointer<ffi.Void> Function(ffi.Pointer<Utf8>);
typedef CacGetClientDart = ffi.Pointer<ffi.Void> Function(ffi.Pointer<Utf8>);

typedef CacFreeClientNative = ffi.Void Function(ffi.Pointer<ffi.Void>);
typedef CacFreeClientDart = void Function(ffi.Pointer<ffi.Void>);

typedef CacGetConfigNative = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>, ffi.Pointer<Utf8>, ffi.Pointer<Utf8>);
typedef CacGetConfigDart = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>, ffi.Pointer<Utf8>, ffi.Pointer<Utf8>);
typedef CacGetDefaultConfigNative = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>, ffi.Pointer<Utf8>);
typedef CacGetDefaultConfigDart = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>, ffi.Pointer<Utf8>);

typedef CacGetLastModifiedNative = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>);

typedef CacFreeStringNative = ffi.Void Function(ffi.Pointer<Utf8>);
typedef CacFreeStringDart = void Function(ffi.Pointer<Utf8>);

typedef CacLastErrorMessageNative = ffi.Pointer<Utf8> Function();
typedef CacLastErrorMessageDart = ffi.Pointer<Utf8> Function();

typedef CacGetResolvedConfigsNative = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>,
    ffi.Pointer<Utf8>,
    ffi.Pointer<Utf8>,
    ffi.Pointer<Utf8>);
typedef CacGetResolvedConfigsDart = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>,
    ffi.Pointer<Utf8>,
    ffi.Pointer<Utf8>,
    ffi.Pointer<Utf8>);

typedef CacStartPollingUpdateNative = ffi.Pointer<ffi.Void> Function(
    ffi.Pointer<Utf8>);
typedef CacStartPollingUpdateDart = ffi.Pointer<ffi.Void> Function(
    ffi.Pointer<Utf8>);

typedef CacGetLastModifiedDart = ffi.Pointer<Utf8> Function(
    ffi.Pointer<ffi.Void>);
