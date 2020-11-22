args@{ config, lib, pkgs, ... }:

with lib; let
  cfg = config.programs.emacs;

  forEachPackage = f: lib.flatten (lib.mapAttrsToList (k: v:
    let ret = if v.enable then f v else [];
    in if builtins.isNull ret then [] else ret
  ) config.emacs-loader);

  packageDeps = forEachPackage (p: p.package cfg.package.pkgs);
  systemDeps = forEachPackage (p: p.systemDeps);

  emacsWrapper = pkgs.writeShellScriptBin "ec" ''
    if test "$(systemctl --user show -p ActiveState --value emacs)" != "active"; then
      systemctl --user stop emacs emacs.socket
      systemctl --user start emacs.socket
    fi

    exec emacsclient $@
  '';
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
      nixfmt w3m findutils cmake gnumake gcc libtool gtk3 age emacsWrapper
    ] ++ (with cfg.package.pkgs; [
      leaf auto-compile gcmh diminish epkg log4e bug-hunter use-package
    ]);

    programs.emacs = {
      package = lib.fix (self: pkgs.gccEmacs.overrideAttrs (drv: {
        passthru = {
          pkgs = pkgs.emacsPackagesFor self;
          nativeComp = drv.nativeComp or false;
        };
      }));
      extraPackages = epkgs: forEachPackage (p: p.package epkgs);
    };

    systemd.user.services.emacs = {
     #Service.Type = "notify";
      Service.Restart = lib.mkForce "no";
      Install.WantedBy = lib.mkForce [];
    };

    systemd.user.sockets.emacs = let
      service = config.systemd.user.services.emacs;
    in {
      Unit.Description = "Socket for ${service.Unit.Description}";
      Unit.Documentation = service.Unit.Documentation;
      Socket.ListenStream = "%t/emacs/server";
      Socket.Symlinks = "%h/.emacs.d/server/server";
      Socket.DirectoryMode="700";
      Install.WantedBy = [ "sockets.target" ];
    };
  };
}
