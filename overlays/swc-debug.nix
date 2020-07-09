final: prev: rec {
  swc = prev.velox.swc.overrideAttrs (o: {
    src = ../swc;
    separateDebugInfo = true;
  });
  velox = prev.velox.overrideAttrs (o: {
    passthru = o.passthru // {
      inherit swc;
    };
    separateDebugInfo = true;
  });
}
