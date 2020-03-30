{ pkgs ? import <nixpkgs> { } }:

let
  rebuild = pkgs.writeShellScriptBin "rebuild" ''
    if [ $1 == "tag-current" ]; then
      pathToConfig=$(readlink -f /run/current-system)
    else
      FLAKE=$(git rev-parse --show-toplevel)
      source $(which nixos-rebuild) --flake $FLAKE --use-remote-sudo $@
    fi

    if [ -e "$pathToConfig" ]; then
      SYSTEMPATH=$(basename $pathToConfig)
      PROFILES=($(find /nix/var/nix/profiles/ -lname $pathToConfig))
      echo Tagging $SYSTEMPATH && git tag $SYSTEMPATH || true
      for profile in $PROFILES; do
        SYSTEMNUM=$(hostname)/$(basename $profile)
        echo Tagging $SYSTEMNUM && git tag $SYSTEMNUM || true
      done
    fi
  '';
in pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ git git-crypt nixFlakes rebuild ];

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
