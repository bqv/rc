inputs@{ nyxt, ... }: final: prev: {
  nyxt = prev.nyxt.overrideAttrs (drv: rec {
    src = drv.src.overrideAttrs (drv: {
      src = inputs.nyxt;
      name = final.lib.replaceStrings [drv.meta.version] [version] drv.name;
      installPhase = builtins.replaceStrings ["nyxt-ext"] ["nyxt"] drv.installPhase;
    });
    version = inputs.nyxt.lastModifiedDate;
  });
}
