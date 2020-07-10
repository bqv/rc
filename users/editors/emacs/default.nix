args@{ config, lib, pkgs, ... }:

with lib; let
  cfg = config.programs.emacs;

  myEmacs = pkgs.gccEmacs.override {
    withXwidgets = true;
    webkitgtk = pkgs.large.webkitgtk;
  };

  forEachPackage = f: lib.flatten (lib.mapAttrsToList (k: v:
    let ret = if v.enable then f v else [];
    in if builtins.isNull ret then [] else ret
  ) config.emacs-loader);

  packageDeps = forEachPackage (p: p.package cfg.package.pkgs);
  systemDeps = forEachPackage (p: p.systemDeps);
in {
  imports = [
    ../../../emacs
  ];

  config = mkIf cfg.enable rec {
    home.file = {
      ".emacs.d/early-init.el".source = (import ./early-init.nix args).out;
      ".emacs.d/init.el".source = (import ./init.nix args).out;
    };

    home.packages = with pkgs; packageDeps ++ systemDeps ++ [
      nixfmt w3m findutils cmake gnumake gcc libtool gtk3 age
    ] ++ (with cfg.package.pkgs; [
      leaf auto-compile gcmh diminish epkg log4e bug-hunter use-package
    ]);

    programs.emacs = {
      package = lib.fix (self: myEmacs.overrideAttrs (_: {
        passthru = {
          pkgs = pkgs.emacsPackagesFor self;
        };
      }));
      extraPackages = epkgs: forEachPackage (p: p.package epkgs);
    };

    systemd.user.services.emacs.Service = {
     #Type = "notify";
     #NotifyAccess = "all";
    };
  };
}
