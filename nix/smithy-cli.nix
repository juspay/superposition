{
  lib,
  stdenv,
  fetchzip,
  jdk17,
}:

let
  version = "1.69.0";
  smithy-platform = lib.pipe stdenv.hostPlatform.system (
    with builtins;
    [
      (split "-")
      (filter (x: builtins.typeOf x == "string" && x != ""))
      lib.lists.reverseList
      (concatStringsSep "-")
    ]
  );
  sha256 =
    if smithy-platform == "darwin-aarch64" then
      "sha256-umZfI1bC7uF8fU33DDNSrjNrJq+z8sdwEZ6BxfbOEAg="
    else
      "sha256-j99kyy1XlJwEmDlKJA7G0KmnE6mBgfxoTxWY09opNo4=";
in
stdenv.mkDerivation {
  name = "smithy-cli";
  inherit version;

  src = fetchzip {
    url = "https://github.com/smithy-lang/smithy/releases/download/${version}/smithy-cli-${smithy-platform}.zip";
    inherit sha256;
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    ./install -i $out -b $out/bin
    ## Need this othewise it won't run on nixos machines.
    ## NOTE One can try to patchelf the provided java bin, but this works for now.
    ln -sf ${jdk17}/bin/java $out/bin/java
  '';

  meta = with lib; {
    description = "CLI for the Smithy IDL (Interface Definition Language)";
    homepage = "https://github.com/smithy-lang/smithy";
    license = licenses.asl20;
    platforms = platforms.all;
    maintainers = [ ];
  };
}
