inputs@{ ... }: final: prev: {
  nix-bundle = prev.nix-bundle.override {
    nix = prev.nixFlakes;
  };
}
