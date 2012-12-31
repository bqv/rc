inputs@{ rel2009, ... }: final: prev: {
  inherit (inputs.rel2009.legacyPackages.${final.system}.haskellPackages) pointfree-fancy;
}
