{ pkgs ? import <nixpkgs> {}
, stdenv ? pkgs.stdenv
, lib ? pkgs.lib }:

let
  swc = pkgs.velox.swc.overrideAttrs (o: {
    src = ./.;
    makeFlags = o.makeFlags + " ENABLE_XWAYLAND=1";
    separateDebugInfo = true;
  });
  velox = pkgs.velox.overrideAttrs (o: {
    buildInputs = (lib.filter (d: (d.version or "") != "1fe3b4d") o.buildInputs)
      ++ [ (pkgs.hiPrio swc) ];
  });
in pkgs.symlinkJoin {
  name = "result";
  paths = [ swc velox ];
}
