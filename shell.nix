{ nixpkgs ? if (builtins ? getFlake) then builtins.getFlake "nixpkgs" else <nixpkgs>, system ? builtins.currentSystem }:

let
  flakeFile = import ./flake.nix;
  flake = flakeFile.outputs {
    master = nixpkgs;
  };
in flake.devShell.${system}
