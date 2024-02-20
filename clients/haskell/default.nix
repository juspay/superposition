{
  perSystem = { config, pkgs, self', ... }: {
    haskellProjects.default = {
      projectRoot = ./.;
      autoWire = [ "packages" "checks" "apps" ];
      settings = {
        cac_client.custom = _: self'.packages.superposition;
        experimentation_client.custom = _: self'.packages.superposition;
      };
    };

    devShells.haskell = pkgs.mkShell {
      name = "superposition-haskell-clients";
      shellHook = ''
        export LIBRARY_PATH=${self'.packages.superposition}/lib
      '';
      inputsFrom = [
        config.haskellProjects.default.outputs.devShell
      ];
    };
  };
}
