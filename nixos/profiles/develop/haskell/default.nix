{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    cabal-install
    stack
    ghc haskellPackages.ghcide
    cabal2nix
    haskellPackages.hoogle
    haskellPackages.pointfree-fancy
  ];
}
