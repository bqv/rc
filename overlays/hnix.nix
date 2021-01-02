inputs@{ hnix-overlay, ... }: final: prev: {
  hnix = let
    inherit (import final.path {
      inherit (final) system;
      overlays = import (inputs.hnix-overlay + "/overlay.nix");
    }) haskellPackages haskell;
    hnix = haskell.lib.appendPatch haskellPackages.hnix (
      final.fetchpatch {
        url = "https://github.com/haskell-nix/hnix/commit/e2ad934492eeac9881527610e4a1c1cf31ea1115.patch";
        sha256 = "dWMf50asrDaYtAU+Ii/Eu7/HiGnae0aeVqh7iUUhjr4=";
      }
    );
  in final.writeScriptBin "hnix" ''
    #!${final.execline}/bin/execlineb -S0
    export NIX_DATA_DIR ${hnix.src}/data
    ${hnix}/bin/hnix -I nix=${inputs.nix}/corepkgs $@
  '';
}
