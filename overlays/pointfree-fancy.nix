inputs@{ rel2009, ... }: final: prev: {
  haskellPackages = prev.haskellPackages // {
    inherit (inputs.rel2009.haskellPackages) pointfree-fancy;
  };
}
