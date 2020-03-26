{ pkgs ? import <nixpkgs> { } }:
let
  configs = "${toString ./.}#nixosConfigurations";
  hostname = pkgs.lib.fileContents /etc/hostname;
  build = "config.system.build";

  rebuild = pkgs.writeShellScriptBin "rebuild" ''
    if [[ -z $1 ]]; then
      echo "Usage: $0 [--show-trace] [host] {switch|boot|test|iso}"
    else
      NIX="nix"
      if [[ $1 == "--show-trace" ]]; then
        NIX="nix -vv $1"
        shift
      fi

      INITIAL_SYSTEM="$(readlink -f /run/current-system)"
      if [[ $1 == "iso" ]]; then
        $NIX build ${configs}.niximg.${build}.isoImage
      elif [[ -z $2 ]]; then
        sudo -E $NIX run ${configs}.${hostname}.${build}.toplevel -c switch-to-configuration $1
        if [[ $INITIAL_SYSTEM != "$(readlink -f /run/current-system)" ]] \
        && [[ $1 == "switch" ]]; then
          sudo -E nix-env -p /nix/var/nix/profiles/system --set /run/current-system
          sudo -E $NIX run ${configs}.${hostname}.${build}.toplevel -c switch-to-configuration boot
        fi
      else
        sudo -E $NIX run -vv ${configs}.$1.${build}.toplevel -c switch-to-configuration $2
      fi
      FINAL_SYSTEM="$(readlink -f /run/current-system)"

      if [[ "$INITIAL_SYSTEM" != "$FINAL_SYSTEM" ]]; then
        git tag $(basename $FINAL_SYSTEM)
      else
        echo Not tagging "$FINAL_SYSTEM", no change from "$INITIAL_SYSTEM"
      fi
    fi
  '';
in pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ git git-crypt nixFlakes rebuild ];

  shellHook = ''
    mkdir -p secrets
  '';

  NIX_CONF_DIR = let
    current = pkgs.lib.optionalString (builtins.pathExists /etc/nix/nix.conf)
      (builtins.readFile /etc/nix/nix.conf);

    nixConf = pkgs.writeTextDir "opt/nix.conf" ''
      ${current}
      experimental-features = nix-command flakes ca-references
    '';
  in "${nixConf}/opt";
}
