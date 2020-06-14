{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    cabal-install
    stack
    ghc hies
  ];
}
