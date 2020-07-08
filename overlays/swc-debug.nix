final: prev: {
  velox = prev.velox.overrideAttrs (o: {
    passthru = o.passthru // {
      swc = o.passthru.swc.overrideAttrs (o: {
        src = ../swc;
        separateDebugInfo = true;
      });
    };
    separateDebugInfo = true;
  });
}
