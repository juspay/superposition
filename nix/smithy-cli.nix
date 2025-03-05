{
  lib,
  stdenv,
  fetchzip,
  jdk17,
}:

let
  version = "1.55.0";
  smithy-platform = lib.pipe stdenv.hostPlatform.system (
    with builtins;
    [
      ## Converts 'x86_64-linux' into 'linux-x86_64'.
      (split "-")
      (filter (x: builtins.typeOf x == "string" && x != ""))
      lib.lists.reverseList
      (concatStringsSep "-")
    ]
  );
  sha256 =
    if smithy-platform == "darwin-aarch64" then
      "sha256-08uJuB9We4EVlxt7gh07/mxYZqma9B7VZ/vflposwZU="
    ## FIXME Defaulting to linux-x86_64 SHA.
    else
      "sha256-L4xWsgzGOIbfT9fTFyEjZBIo7GA2cn10/m6YzJjWxcU=";
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
