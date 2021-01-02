inputs@{ hnix-overlay, ... }: final: prev: {
  hnix = let
    inherit (inputs.haskups.legacyPackages.${final.system}) haskellPackages haskell;
  in haskell.lib.overrideSrc (haskellPackages.override {
    overrides = self: super: {
      attoparsec = haskell.lib.dontCheck super.attoparsec;
      cassava = haskell.lib.dontCheck super.cassava;
      hnix-store-core = self.hnix-store-core_0_4_0_0;
      hnix-store-remote = haskell.lib.unmarkBroken super.hnix-store-remote;
      ListLike = super.ListLike.overrideAttrs (_:{ patches = []; });
      algebraic-graphs = haskell.lib.dontCheck super.algebraic-graphs;
      nix-derivation = haskell.lib.dontCheck super.nix-derivation;
      hnix = haskell.lib.addBuildDepend super.hnix self.hnix-store-remote;
    };
  }).hnix {
    src = final.runCommand "hnix-src" {
      cnix = final.fetchFromGitHub {
        owner = "haskell-nix";
        repo = "nix";
        rev = "61e816217bfdfffd39c130c7cd24f07e640098fc";
        sha256 = "19d0r6vllr36s7a62g6dqfx698xl3sg1fymh5mf8vkvj572v4z27";
        # date = 2019-10-10T15:03:46+02:00;
      };
      hnix = inputs.hnix-overlay;
    } ''
      cp -rv $hnix $out
      chmod a+w $out/data/nix
      cp -rv $cnix/corepkgs $out/data/nix/
    '';
  };
}
