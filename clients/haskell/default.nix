{
  perSystem =
    {
      config,
      pkgs,
      self',
      ...
    }:
    let hpkgs = pkgs.haskell.packages.ghc964;
    in {
      haskellProjects.default = {
        basePackages = hpkgs;
        projectRoot = ./.;
        autoWire = [
          "packages"
          "checks"
          "apps"
        ];
        settings = {
          cac_client.custom = _: self'.packages.cac_client;
          experimentation_client.custom = _: self'.packages.experimentation_client;
          superposition_core.custom = _: self'.packages.superposition_core;
        };
      };
      devShells.haskell = pkgs.mkShell {
        name = "superposition-haskell-clients";
        shellHook = ''
            export LIBRARY_PATH=${self'.packages.superposition_core}/lib:${self'.packages.superposition}/lib
            export C_INCLUDE_PATH=${self'.packages.superposition_core}/include
        '';
        inputsFrom = [
          config.haskellProjects.default.outputs.devShell
        ];
      };
    };
}
