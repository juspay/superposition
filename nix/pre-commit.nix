{ ... }:
{
  perSystem = { pkgs, lib, ... }: {
    pre-commit = {
      check.enable = true;
      settings = {
        hooks = {
          eclint.enable = false;
        };
      };
    };
  };
}
