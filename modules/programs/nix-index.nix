{ config, lib, pkgs, inputs, ... }:

with lib;

let
  cfg = config.programs.nix-index;
in

{
  options.programs.nix-index = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable nix-index and it's command-not-found helper.";
    };

    package = mkOption {
      type = types.package;
      default = pkgs.nix-index;
      defaultText = "pkgs.nix-index";
      description = "This option specifies the nix-index package to use.";
    };
  };


  config = mkIf cfg.enable {

    environment.systemPackages = [ cfg.package ];

    environment.interactiveShellInit = "source ${cfg.package}/etc/profile.d/command-not-found.sh";

    programs.command-not-found.dbPath = "${inputs.nixexprs}/programs.sqlite";

  };
}
