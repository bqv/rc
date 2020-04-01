{ pkgs ? import <nixpkgs> { } }:

let
  rebuild = pkgs.writeShellScriptBin "rebuild" ''
    REV=$(git rev-parse HEAD)
    if [ $1 == "tag-current" ]; then
      pathToConfig=$(readlink -f /run/current-system)
    else
      FLAKE=$(git rev-parse --show-toplevel)
      source $(which nixos-rebuild) --flake $FLAKE --use-remote-sudo $@
    fi

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
in pkgs.mkShell {
  nativeBuildInputs = with pkgs; let
    git-crypt = pkgs.git-crypt.overrideAttrs (attrs: rec {
      worktreePatch = fetchurl {
        url = "https://github.com/AGWA/git-crypt/files/2771938/git-crypt-support-worktree-simple-version-patch.txt";
        sha256 = "1k477m6g3zjdarjr38lndh0kpgkp0yi8lg2iqdispfd4c85krrax";
      };
      patches = [ worktreePatch ];
    });
  in [ git git-crypt nixFlakes rebuild ];

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
