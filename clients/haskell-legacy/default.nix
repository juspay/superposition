{
  perSystem = { config, pkgs, self', ... }: {
    haskellProjects.legacy = {
      projectRoot = ./.;
      autoWire = [ "packages" "checks" "apps" ];
      settings = {
        cac_client.custom = _: self'.packages.cac_client;
        experimentation_client.custom = _: self'.packages.experimentation_client;
      };
    };

    devShells.haskell-legacy = pkgs.mkShell {
      name = "superposition-haskell-clients";
      shellHook = ''
        export LIBRARY_PATH=${self'.packages.superposition}/lib
      '';
      inputsFrom = [
        config.haskellProjects.legacy.outputs.devShell
      ];
    };
  };
}
