{
  perSystem = { config, pkgs, self', ... }: {
    haskellProjects.default = {
      projectRoot = ./.;
      autoWire = [ "packages" "checks" "apps" ];
      settings = {
        cac_client.custom = _: self'.packages.superposition;
        superposition_client.custom = _: self'.packages.superposition;
      };
    };

    devShells.haskell = pkgs.mkShell {
      name = "superposition-haskell-clients";
      inputsFrom = [
        config.haskellProjects.default.outputs.devShell
      ];
      shellHook = ''
        export LIBRARY_PATH=${self'.packages.superposition}/lib
      '';
    };
  };
}
