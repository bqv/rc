{ config, lib, pkgs, ... }:

let
  cfg = config.programs.nix-index;
in {
  options.programs.nix-index = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether to enable nix-index and it's command-not-found helper.";
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.nix-index;
      defaultText = "pkgs.nix-index";
      description = "This option specifies the nix-index package to use.";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    environment.interactiveShellInit = "source ${cfg.package}/etc/profile.d/command-not-found.sh";

    programs.command-not-found.enable = false;
  };
}
