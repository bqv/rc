final: prev: {
  velox = prev.velox.overrideAttrs (o: {
    passthru = o.passthru // {
      swc = o.passthru.swc.overrideAttrs (o: {
        separateDebugInfo = true;
      });
    };
    separateDebugInfo = true;
  });
}
