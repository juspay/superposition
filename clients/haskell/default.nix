{
  perSystem = { config, pkgs, self', ... }: {
    haskellProjects.default = {
      basePackages = pkgs.haskell.packages.ghc964;
      projectRoot = ./.;
      autoWire = [ "packages" "checks" "apps" ];
      packages = {
        SuperpositionSDK.source = ./sdk;
      };
    };
    devShells.haskell = pkgs.mkShell {
      inputsFrom = [
        config.haskellProjects.default.outputs.devShell
      ];
    };
  };
}
