{
  perSystem =
    {
      config,
      pkgs,
      self',
      ...
    }:
    {
      haskellProjects.default = {
        basePackages = pkgs.haskell.packages.ghc964;
        projectRoot = ./.;
        autoWire = [
          "packages"
          "checks"
          "apps"
        ];
        settings = {
          cac_client.custom = _: self'.packages.cac_client;
          experimentation_client.custom = _: self'.packages.experimentation_client;
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
