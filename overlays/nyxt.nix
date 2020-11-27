inputs@{ nyxt, ... }: final: prev: {
  nyxt = prev.nyxt.overrideAttrs (drv: rec {
    src = drv.src.overrideAttrs (drv: {
      src = inputs.nyxt;
      name = final.lib.replaceStrings [drv.meta.version] [version] drv.name;
      installPhase = builtins.replaceStrings ["nyxt-ext"] ["nyxt"] drv.installPhase;
    });
    version = inputs.nyxt.lastModifiedDate;
  });

  nyxtFor = lisp: let
    inherit (final) lib;
    clwrapper = final.wrapLisp lisp;

    quicklispPackages = final.quicklispPackagesFor clwrapper;
    lispPackages = (final.lispPackagesFor clwrapper).override {
      pkgs = final.extend (self: super: {
        lispPackages = quicklispPackages // lispPackages;
      });
    };

    deepOverride = t: p: p.overrideAttrs (drv: {
      propagatedBuildInputs = map (q: if q == null then q
                                      else if q.name == t.name then t
                                      else deepOverride t q)
                                  (drv.propagatedBuildInputs or []);
    });

    iterate' = quicklispPackages.iterate.overrideAttrs (drv: {
      nativeBuildInputs = [ quicklispPackages.rt ];
    });
    cl-syslog' = lispPackages.buildLispPackage {
      baseName = "cl-syslog";
      version = "2006-11-28";
      description = "cl-syslog";
      deps = [ quicklispPackages.cffi ];
      buildSystems = [ "cl-syslog" ];
      src = final.fetchFromGitHub {
        owner = "lhope";
        repo = "cl-syslog";
        rev = "f62d524";
        sha256 = "rH0zOgXR3BixtVaqcYPhIfZBjqYgWnqrORQfr1mBqFM=";
      };
      packageName = "cl-syslog";
      asdFilesToKeep = [ "cl-syslog.asd" ];
    };
    log4cl' = quicklispPackages.log4cl.overrideAttrs (drv: {
      nativeBuildInputs = [ cl-syslog' ];
    });
    nyxt' = (quicklispPackages // lispPackages).nyxt.overrideAttrs (drv: {
      src = inputs.nyxt;
      name = lib.replaceStrings [
        drv.meta.version
      ] [
        inputs.nyxt.lastModifiedDate
      ] drv.name;
      installPhase = lib.replaceStrings ["nyxt-ext"] ["nyxt"] drv.installPhase;
    });

    nyxtPkgs.nyxt = (lib.foldl (lib.flip deepOverride) nyxt' [
      log4cl' iterate'
    ]).overrideAttrs (drv: {
      postInstall = lib.replaceStrings [
        "sb-alien::*shared-objects*"
      ] [
        "(setf cffi:*foreign-library-directories*
           (cffi::explode-path-environment-variable
             \\\"NIX_LISP_LD_LIBRARY_PATH\\\"))"
      ] drv.postInstall;
    });
  in (final.nyxt.override { lispPackages = nyxtPkgs; }).overrideAttrs (_: {
    postFixup = '' ln -s ${nyxtPkgs.nyxt} $out/src '';
    version = inputs.nyxt.lastModifiedDate;
  });

  nyxtCCL = final.nyxtFor final.ccl;
}
