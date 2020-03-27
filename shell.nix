{ pkgs ? import <nixpkgs> { } }:
let
  configs = "${toString ./.}#nixosConfigurations";
  hostname = pkgs.lib.fileContents /etc/hostname;
  build = "config.system.build";

  rebuild = pkgs.writeShellScriptBin "rebuild" ''
    INITIAL_SYSTEM="$(readlink -f /run/current-system)"
    sudo nixos-rebuild --flake $PWD $@
    FINAL_SYSTEM="$(readlink -f /run/current-system)"

    if [[ "$INITIAL_SYSTEM" != "$FINAL_SYSTEM" ]]; then
      git tag $(basename $FINAL_SYSTEM)
    else
      echo Not tagging "$FINAL_SYSTEM", no change from "$INITIAL_SYSTEM"
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
