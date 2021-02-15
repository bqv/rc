args@{ config, lib, pkgs, ... }:

with lib; let
  cfg = config.programs.emacs;

  forEachPackage = f: lib.flatten (lib.mapAttrsToList (k: v:
    let ret = if v.enable then f v else [];
    in if builtins.isNull ret then [] else ret
  ) config.emacs.loader);

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

    home.packages = with pkgs; systemDeps ++ [
      nixfmt w3m findutils cmake gnumake gcc libtool gtk3 age emacsWrapper
      (hiPrio emacsPgtkGccClient)
    ] ++ (with cfg.package.pkgs; [
      leaf auto-compile gcmh diminish epkg log4e bug-hunter use-package
    ]);

    emacs.package = cfg.package;

    programs.emacs = rec {
      package = pkgs.emacsPgtkGcc;
      extraPackages = epkgs:
        forEachPackage (p: p.package epkgs) ++
        forEachPackage (p: p.initPkg epkgs) ++
        forEachPackage (p: p.configPkg epkgs);
    };

    systemd.user.services.emacs = {
     #Service.Type = "notify";
      Service.Restart = lib.mkForce "no";
      Service.Environment = "EDITOR=${cfg.package}/bin/emacsclient";
      Service.ExecStopPost = "${pkgs.writeShellScript "emacs-relink-socket" ''
        rm ~/.emacs.d/server/server
        ln -sf /run/user/1000/emacs/server ~/.emacs.d/server/server
      ''}";
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
