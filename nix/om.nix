{
  flake.om.ci.default = {
    root = {
      dir = ".";
      steps = {
        # Users can define custom steps to run any arbitrary flake app or devShell command.
        custom = {
          # This equivalent to `nix run .#hs-cac-client`
          hs-cac-client = {
            type = "app";
            name = "hs-cac-client";
          };

          hs-exp-client = {
            type = "app";
            name = "hs-exp-client";
          };
        };
      };
    };
  };
}
