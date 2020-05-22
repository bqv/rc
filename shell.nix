{ nixpkgs ? if (builtins ? getFlake) then builtins.getFlake "nixpkgs" else <nixpkgs>
, pkgs ? import nixpkgs { }
, my ? import ./pkgs pkgs pkgs
}:

let
  nixos = import ./lib/nixos.nix { pkgs = pkgs // my; };
  flake-shell = import ./lib/flake-shell.nix { inherit pkgs; };
in pkgs.mkShell {
  nativeBuildInputs = with pkgs; let
    git-crypt = pkgs.git-crypt.overrideAttrs (attrs: rec {
      worktreePatch = fetchurl {
        name = "support-worktree-simple-version.patch";
        url = "https://github.com/AGWA/git-crypt/files/2771938/git-crypt-support-worktree-simple-version-patch.txt";
        sha256 = "1k477m6g3zjdarjr38lndh0kpgkp0yi8lg2iqdispfd4c85krrax";
      };
      patches = [ worktreePatch ];
    });
  in [ git git-crypt git-secrets age rage nixfmt flake-shell nixos ];

  shellHook = ''
    mkdir -p secrets
  '';

  NIX_CONF_DIR = with pkgs; let
    nixConf = ''
      ${lib.optionalString (builtins.pathExists /etc/nix/nix.conf)
        (builtins.readFile /etc/nix/nix.conf)}
      experimental-features = nix-command flakes ca-references
    '';
  in linkFarm "nix-conf-dir" ( [ {
    name = "nix.conf";
    path = writeText "flakes-nix.conf" nixConf;
  } ] ++ ( lib.optional (builtins.pathExists /etc/nix/registry.json) {
    name = "registry.json";
    path = /etc/nix/registry.json;
  } ) ++ ( lib.optional (builtins.pathExists /etc/nix/machines) {
    name = "machines";
    path = /etc/nix/machines;
  } ) );
}
