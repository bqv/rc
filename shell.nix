{ pkgs ? import <nixpkgs> { }, my ? import ./pkgs pkgs pkgs }:

let
  shFlagsRules = rules: ''
    . ${my.shflags}
    ${rules}
    FLAGS "$@" || exit $?
    eval set -- "''${FLAGS_ARGV}"
  '';
  rebuild = operation: ''
    REV=$(git rev-parse HEAD)
    FLAKE=$(git rev-parse --show-toplevel)

    ARGS="--use-remote-sudo"

    if [ -z "$FLAGS_host" ]; then
      ARGS="$ARGS --flake $FLAKE"
    else
      ARGS="$ARGS --flake $FLAKE#$FLAGS_host"
    fi

    if [ $FLAGS_showtrace -eq $FLAGS_TRUE ]; then
      ARGS="$ARGS --show-trace"
    fi

    echo '> nixos-rebuild' $ARGS ${operation}
    source $(which nixos-rebuild) $ARGS ${operation}
  '';
  tag = ''
    SYSTEMPATH=$(basename $pathToConfig)
    PROFILES=($(find /nix/var/nix/profiles/ -lname $pathToConfig))
    echo Tagging $SYSTEMPATH && git tag $SYSTEMPATH $REV || true
    for profile in $PROFILES; do
      SYSTEMNUM=$(hostname)/$(basename $profile)
      echo Tagging $SYSTEMNUM && git tag $SYSTEMNUM $REV || true
    done
  '';
  activate = pkgs.writeShellScriptBin "activate" ''
    ${shFlagsRules ''
      DEFINE_string 'host' "" 'Host to build' 'H'
      DEFINE_boolean 'showtrace' false 'Show verbose traces' 't'
    ''}
    ${rebuild "switch"}

    if [ -e "$pathToConfig" ]; then
      ${tag}
    fi
  '';
  dry-boot = pkgs.writeShellScriptBin "dry-boot" ''
    ${shFlagsRules ''
      DEFINE_string 'host' "" 'Host to build' 'H'
      DEFINE_boolean 'showtrace' false 'Show verbose traces' 't'
    ''}
    ${rebuild "test"}

    if [ -e "$pathToConfig" ]; then
      SYSTEMPATH=$(basename $pathToConfig)
      echo Tagging $SYSTEMPATH && git tag $SYSTEMPATH $REV || true
    fi
  '';
  tag-current = pkgs.writeShellScriptBin "tag-current" ''
    REV=$(git rev-parse HEAD)
    pathToConfig=$(readlink -f /run/current-system)
    ${tag}
  '';
  boot = pkgs.writeShellScriptBin "boot" ''
    ${shFlagsRules ''
      DEFINE_string 'host' "" 'Host to build' 'H'
      DEFINE_boolean 'showtrace' false 'Show verbose traces' 't'
    ''}
    ${rebuild "boot"}
  '';
  dry-activate = pkgs.writeShellScriptBin "dry-activate" ''
    ${shFlagsRules ''
      DEFINE_string 'host' "" 'Host to build' 'H'
      DEFINE_boolean 'showtrace' false 'Show verbose traces' 't'
    ''}
    ${rebuild "dry-activate"}
  '';
  dry-build = pkgs.writeShellScriptBin "dry-build" ''
    ${shFlagsRules ''
      DEFINE_string 'host' "" 'Host to build' 'H'
      DEFINE_boolean 'showtrace' false 'Show verbose traces' 't'
    ''}
    ${rebuild "dry-build"}
  '';
  flake-shell = pkgs.writeShellScriptBin "nixFlakes-shell" ''
    nix-shell -E 'with import "${pkgs.path}/nixos" { configuration.nix.package = (import <nixpkgs> {}).nixFlakes; }; pkgs.mkShell { buildInputs = with config.system.build; with pkgs; [ nixos-rebuild ]; }' $@
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
  in [ git git-crypt git-secrets nixFlakes nixfmt flake-shell
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
