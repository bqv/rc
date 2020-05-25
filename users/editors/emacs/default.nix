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

    home.packages = with pkgs; [
      emacs-all-the-icons-fonts nixfmt
    ] ++ packageDeps ++ systemDeps ++ [
      git fish w3m findutils
      cmake gnumake gcc libtool libvterm gtk3 rls age rust-analyzer
    ] ++ (with cfg.package.pkgs; [
      use-package auto-compile gcmh diminish epkg log4e bug-hunter
    ]);

    programs.emacs.package = let
      daemonScript = pkgs.writeShellScript "emacs-wrapper" ''
        if [[ "$@" =~ "--fg-daemon" ]] || [[ "$@" =~ "--daemon" ]]; then
          echo Redirecting output to journal tag: emacs
          systemd-cat -t emacs ${myEmacs}/bin/emacs ''${@//--daemon/--fg-daemon} & disown
          while [[ "$(${myEmacs}/bin/emacsclient --eval 'init-done' 2>&1)" != "t" ]]; do
            sleep 1
          done
        else
          exec ${myEmacs}/bin/emacs $@
        fi
      '';
    in pkgs.stdenv.mkDerivation {
      name = "${myEmacs.name}-wrapped";
      passthru = with pkgs; {
        unwrapped = myEmacs;
        pkgs = emacsPackagesFor myEmacs;
      };
      buildInputs = with pkgs; [ makeWrapper ];
      phases = [ "installPhase" ];
      installPhase = ''
        mkdir -p $out/bin/
        makeWrapper ${daemonScript} $out/bin/emacs \
          --argv0 emacs
        cp -rus ${myEmacs}/* $out/
        rm -f $out/bin/emacs-w64
      '';
    };
  };
}
