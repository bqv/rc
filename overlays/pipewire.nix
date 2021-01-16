inputs@{...}: final: prev: {
  pipewire = if final.lib.versionAtLeast prev.pipewire.version "0.3.20"
  then builtins.trace "pkgs.pipewire: overlay expired" prev.pipewire
  else prev.pipewire.overrideAttrs (drv: {
    version = "0.3.19-git";
    src = final.fetchgit {
      url = drv.src.meta.homepage;
      rev = "5f66650f7c99545369895539aa0824d285d39370";
      sha256 = "c9amvmxU03P14NygJK2hTNR0lTdFK7eEj1eoqv2FTDs=";
    };
  });
}
