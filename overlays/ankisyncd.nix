inputs@{ anki-sync, ... }: final: prev: {
  # Nixpkgs version uses tsudoko's repo, which is woefully out of date
  ankisyncd = prev.ankisyncd.overrideAttrs (_: {
    src = inputs.anki-sync;
  });
}
