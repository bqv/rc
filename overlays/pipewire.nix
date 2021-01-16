inputs@{...}: final: prev: {
  pipewire = if final.lib.versionAtLeast prev.pipewire.version "0.3.20"
  then builtins.trace "pkgs.pipewire: overlay expired" prev.pipewire
  else prev.pipewire.overrideAttrs (drv: {
    version = "0.3.19-git";
    src = final.fetchgit {
      url = drv.src.meta.homepage;
      rev = "5f66650f7c99545369895539aa0824d285d39370";
      sha256 = "daLkYJEjNqsicgC0JJHigRzmBVz0phjDn4o1HYyPvX0=";
    };
  });
}
