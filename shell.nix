{ pkgs ? import <nixpkgs> { } }:

let
  flake-shell = pkgs.writeShellScriptBin "nixFlakes-shell" ''
    nix-shell -E 'with import "${pkgs.path}/nixos" { configuration.nix.package = (import <nixpkgs> {}).nixFlakes; }; pkgs.mkShell { buildInputs = with config.system.build; with pkgs; [ nixos-rebuild ]; }' $@
  '';
  activate = pkgs.writeShellScriptBin "activate" ''
    REV=$(git rev-parse HEAD)
    FLAKE=$(git rev-parse --show-toplevel)
    source $(which nixos-rebuild) --flake $FLAKE --use-remote-sudo switch $@

    if [ -e "$pathToConfig" ]; then
      SYSTEMPATH=$(basename $pathToConfig)
      PROFILES=($(find /nix/var/nix/profiles/ -lname $pathToConfig))
      echo Tagging $SYSTEMPATH && git tag $SYSTEMPATH $REV || true
      for profile in $PROFILES; do
        SYSTEMNUM=$(hostname)/$(basename $profile)
        echo Tagging $SYSTEMNUM && git tag $SYSTEMNUM $REV || true
      done
    fi
  '';
  dry-boot = pkgs.writeShellScriptBin "dry-boot" ''
    REV=$(git rev-parse HEAD)
    FLAKE=$(git rev-parse --show-toplevel)
    source $(which nixos-rebuild) --flake $FLAKE --use-remote-sudo test $@

    if [ -e "$pathToConfig" ]; then
      SYSTEMPATH=$(basename $pathToConfig)
      echo Tagging $SYSTEMPATH && git tag $SYSTEMPATH $REV || true
    fi
  '';
  tag-current = pkgs.writeShellScriptBin "tag-current" ''
    REV=$(git rev-parse HEAD)
    pathToConfig=$(readlink -f /run/current-system)
    SYSTEMPATH=$(basename $pathToConfig)
    PROFILES=($(find /nix/var/nix/profiles/ -lname $pathToConfig))
    echo Tagging $SYSTEMPATH && git tag $SYSTEMPATH $REV || true
    for profile in $PROFILES; do
      SYSTEMNUM=$(hostname)/$(basename $profile)
      echo Tagging $SYSTEMNUM && git tag $SYSTEMNUM $REV || true
    done
  '';
  boot = pkgs.writeShellScriptBin "boot" ''
    REV=$(git rev-parse HEAD)
    FLAKE=$(git rev-parse --show-toplevel)
    source $(which nixos-rebuild) --flake $FLAKE --use-remote-sudo boot $@
  '';
  dry-activate = pkgs.writeShellScriptBin "dry-activate" ''
    REV=$(git rev-parse HEAD)
    FLAKE=$(git rev-parse --show-toplevel)
    source $(which nixos-rebuild) --flake $FLAKE --use-remote-sudo dry-activate $@
  '';
  dry-build = pkgs.writeShellScriptBin "dry-build" ''
    REV=$(git rev-parse HEAD)
    FLAKE=$(git rev-parse --show-toplevel)
    source $(which nixos-rebuild) --flake $FLAKE --use-remote-sudo dry-build $@
  '';
in pkgs.mkShell {
  nativeBuildInputs = with pkgs; let
    git-crypt = pkgs.git-crypt.overrideAttrs (attrs: rec {
      worktreePatch = fetchurl {
        url = "https://github.com/AGWA/git-crypt/files/2771938/git-crypt-support-worktree-simple-version-patch.txt";
        sha256 = "1k477m6g3zjdarjr38lndh0kpgkp0yi8lg2iqdispfd4c85krrax";
      };
      patches = [ worktreePatch ];
    });
  in [ git git-crypt git-secrets nixFlakes flake-shell
       activate dry-boot tag-current boot dry-activate dry-build ];

  shellHook = ''
    mkdir -p secrets
  '';

  GC_DONT_GC = 1; # Dangerously mitigate GC-based crashes

  NIX_CONF_DIR = let
    current = pkgs.lib.optionalString (builtins.pathExists /etc/nix/nix.conf)
      (builtins.readFile /etc/nix/nix.conf);

    nixConf = pkgs.writeTextDir "opt/nix.conf" ''
      ${current}
      experimental-features = nix-command flakes ca-references
    '';
  in "${nixConf}/opt";
}
