inputs@{ nyxt, ... }: final: prev: let
  variants = {
    ccl = final.nyxtCCL // variants;
    sbcl = final.nyxtSBCL // variants;
  };
in {
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
        "\"\""
        "='"
      ] [
        "(setf cffi:*foreign-library-directories*
           (cffi::explode-path-environment-variable
             \\\"NIX_LISP_LD_LIBRARY_PATH\\\"))"
        "\"cffi\""
        "='\nset +x"
      ] drv.postInstall;
    });
  in (prev.nyxt.override { lispPackages = nyxtPkgs; }).overrideAttrs (_: {
    postFixup = ''
      head -n -1 $out/bin/nyxt > $out/bin/nyxt-repl
      echo 'exec ${nyxtPkgs.nyxt}/bin/nyxt-lisp-launcher.sh "$@"' >> $out/bin/nyxt-repl
      chmod a+x $out/bin/nyxt-repl
      ln -s ${nyxtPkgs.nyxt} $out/src
    '';
    version = inputs.nyxt.lastModifiedDate;
  });

  nyxtCCL = final.nyxtFor final.ccl;
  nyxtSBCL = final.nyxtFor final.sbcl;

  nyxt = variants.sbcl;
  nyxt-eval = final.writeShellScript "nyxt" ''
    eval $(head -n -1 ${variants.ccl}/bin/nyxt)
    exec ${variants.ccl.src}/bin/nyxt-lisp-launcher.sh \
      -e '(asdf:load-system :nyxt/gtk-application)' \
      -- $@
  '';
  nyxt-live = final.writeScriptBin "nyxt-live" ''
    #!${final.execline}/bin/execlineb -S0
    ${variants.ccl}/bin/nyxt-repl -e "(asdf:load-system :nyxt/gtk)" -e "(nyxt:start)" $@
  '';
}
