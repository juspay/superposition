{ lib
, stdenv
, fetchurl
, jre
, unzip
, jdk17
}:

let
  version = "1.55.0";
  sha256 = "sha256-tbVjJcKymeoglBMyAtKwXGwMu/m4IZuQ7Gnf5YNHG1I=";
  smithy-platform = lib.pipe stdenv.hostPlatform.system (with builtins; [
      ## Converts 'x86_64-linux' into 'linux-x86_64'.
      (split "-")
      (filter (x: builtins.typeOf x == "string" && x != ""))
      lib.lists.reverseList
      (concatStringsSep "-")
  ]);
in
stdenv.mkDerivation {
  name = "smithy-cli";
  inherit version;

  src = fetchurl {
    url = "https://github.com/smithy-lang/smithy/releases/download/${version}/smithy-cli-${smithy-platform}.zip";
    inherit sha256;
  };

  nativeBuildInputs = [ jre unzip ];

  dontConfigure = true;
  dontBuild = true;

  unpackPhase = ''
    unzip $src
  '';

  installPhase = ''
      cd smithy-cli-${smithy-platform} && ./install -i $out -b $out/bin
      ## Need this othewise it won't run on nixos machines.
      ## NOTE One can try to patchelf the provided java bin, but this works for now.
      ln -sf ${jdk17}/bin/java $out/bin/java
  '';

  meta = with lib; {
    description = "CLI for the Smithy IDL (Interface Definition Language)";
    homepage = "https://github.com/smithy-lang/smithy";
    license = licenses.asl20;
    platforms = platforms.all;
    maintainers = with maintainers; [ ];
  };
}
