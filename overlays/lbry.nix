inputs@{ pr96368, ... }: final: prev: {
  lbry = (final.symlinkJoin {
    name = "lbry";
    paths = [
      (final.writeScriptBin "lbry-x11" ''
        #!${final.runtimeShell}
        export DISPLAY=:0
        exec -a lbry ${inputs.pr96368.legacyPackages.${final.system}.lbry}/bin/lbry $@
      '')
      inputs.pr96368.legacyPackages.${final.system}.lbry
    ];
  }).overrideAttrs (_: { inherit (inputs.pr96368.legacyPackages.${final.system}.lbry) meta; });
}
