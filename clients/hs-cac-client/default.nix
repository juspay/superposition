{
  perSystem = { config, pkgs, lib, self', ... }: {
    haskellProjects.cac = {
      projectRoot = ./.;
      autoWire = [ "packages" "checks" "apps" ];
      settings = {
        cac_client.custom = _: self'.packages.clients;
      };
    };
    packages.hs-cac-client = self'.packages.cac-hs-cac-client.overrideAttrs (oa: {
      cac_client = self'.packages.clients;
      nativeBuildInputs = (oa.nativeBuildInputs or [ ]) ++ [
        pkgs.makeWrapper
      ];
      # https://gist.github.com/CMCDragonkai/9b65cbb1989913555c203f4fa9c23374
      postFixup = (oa.postFixup or "") + ''
        wrapProgram $out/bin/* \
          --set ${if pkgs.stdenv.isLinux then "LD_LIBRARY_PATH" else "DYLD_LIBRARY_PATH"} $cac_client/lib
      '';
    });
    apps.cac.program = lib.getExe self'.packages.cac;

    devShells.cac-client = pkgs.mkShell {
      name = "hs-cac-client";
      inputsFrom = [
        config.haskellProjects.default.outputs.devShell
        config.flake-root.devShell
      ];
      # TODO: set once, based on platform
      # TODO(refactor): SRID: can we do this in one place?
      # LD_LIBRARY_PATH = "${self'.packages.cac_client}/lib";
      shellHook = ''
        export DYLD_LIBRARY_PATH="${self'.packages.clients}/lib"
      '';
    };
  };
}
