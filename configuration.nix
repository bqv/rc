# this file is an impure recreation of the flake profile currently deployed
# based on the systems hostname. The purpose is so tools which do not yet have
# flake support (e.g `nixos-option`), can work as expected.
{ lib, pkgs, ... }:
let
  inherit (builtins) attrNames readDir;

  hostname = lib.fileContents /etc/hostname;
  host = "/etc/nixos/hosts/${hostname}";
  config = if (builtins.pathExists host) then
    [ host ]
  else
    [ /etc/nixos/hosts/NixOS.nix ];
in {
  imports = (import ./modules/list.nix) ++ [
    (import "${builtins.fetchTarball "https://github.com/rycee/home-manager/archive/bqv-flakes.tar.gz"}/nixos" pkgs.path)
    ./profiles/core.nix
  ] ++ config;

  networking.hostName = hostname;
  nix.nixPath = [
    "nixpkgs=${<nixpkgs>}"
    "nixos-config=/etc/nixos/configuration.nix"
    "nixpkgs-overlays=/etc/nixos/overlays"
  ];

  nixpkgs.overlays = let
    overlays = map (name: import (./overlays + "/${name}"))
      (attrNames (readDir ./overlays));
    commonArgs = pkgs: { inherit config pkgs; };
  in overlays ++ [
    (final: prev: { master = import (builtins.fetchTarball
      "https://github.com/nixos/nixpkgs/archive/master.tar.gz") (commonArgs prev);
    })
    (final: prev: { staged = import (builtins.fetchTarball
      "https://github.com/nixos/nixpkgs/archive/staging.tar.gz") (commonArgs prev);
    })
    (final: prev: { small = import (builtins.fetchTarball
      "https://github.com/nixos/nixpkgs/archive/nixos-unstable-small.tar.gz") (commonArgs prev);
    })
    (final: prev: { large = import (builtins.fetchTarball
      "https://github.com/nixos/nixpkgs/archive/nixos-unstable.tar.gz") (commonArgs prev);
    })
    (final: prev: { nur = import (builtins.fetchTarball
      "https://github.com/nix-community/nur/archive/master.tar.gz") (commonArgs prev);
    })
  ];
}
