{
  config,
  lib,
  pkgs,
  ...
}:
{
  options = {
    services.localstack = {
      enable = lib.mkEnableOption "Enable localstack";
      package = lib.mkPackageOption pkgs "localstack" { };
      message = lib.mkOption {
        type = lib.types.str;
        default = "Localstack";
        description = "Fully functional local Cloud stack";
      };
    };
  };
  config =
    let
      cfg = config.services.localstack;
    in
    lib.mkIf cfg.enable {
      settings.processes.localstack = {
        environment = {
          SERVICES = "s3, sns, sqs, logs, cloudwatch, kms";
          LOCALSTACK_DOCKER_DISABLED = "1";
          DEBUG = "1";
        };
        command = "${lib.getExe cfg.package} start --host";
      };
    };
}
