final: prev: {
  normalize = prev.normalize.overrideAttrs (super: {
    postInstall = "ln -s $out/bin/normalize $out/bin/normalize-audio";
  });
  gpodder = prev.gpodder.overrideAttrs (super: {
    propagatedBuildInputs = super.propagatedBuildInputs ++ (with final.python3Packages; [ mutagen ]);
  });
}
