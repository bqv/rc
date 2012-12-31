inputs@{ self, ... }: final: prev: {
  flake = self;
} // import ../pkgs final prev
