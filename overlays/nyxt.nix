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

    eager-future2 = lispPackages.buildLispPackage {
      baseName = "eager-future2";
      version = "2011-02-07";
      description = "eager-future2";
      deps = [
        quicklispPackages.bordeaux-threads
        quicklispPackages.trivial-garbage
      ];
      buildSystems = [ "eager-future2" ];
      src = final.fetchzip {
        url = "https://common-lisp.net/project/eager-future/release/eager-future2-0.2.tgz";
        sha256 = "lkLFUkTgWoOd9AMLJkqpYqP1pgpSyO4xJ4ueFm3PuFE=";
      };
      packageName = "eager-future2";
      asdFilesToKeep = [ "eager-future2.asd" ];
    };
    jpl-util = lispPackages.buildLispPackage {
      baseName = "jpl-util";
      version = "2011-06-14";
      description = "jpl-util";
      deps = [];
      buildSystems = [ "jpl-util" ];
      src = final.fetchFromGitHub {
        owner = "hawkir";
        repo = "cl-jpl-util";
        rev = "0311ed374e19a49d43318064d729fe3abd9a3b62";
        sha256 = "0nc0rk9n8grkg3045xsw34whmcmddn2sfrxki4268g7kpgz0d2yz";
        # date = 2015-10-05T11:08:40-07:00;
      };
      packageName = "jpl-util";
      asdFilesToKeep = [ "jpl-util.asd" ];
    };
    jpl-queues = lispPackages.buildLispPackage {
      baseName = "jpl-queues";
      version = "2009-10-19";
      description = "jpl-queues";
      deps = [
        quicklispPackages.bordeaux-threads
        jpl-util
      ];
      buildSystems = [ "jpl-queues" ];
      src = final.fetchzip {
        url = "http://www.thoughtcrime.us/software/jpl-queues/jpl-queues-0.1.tar.gz";
        sha256 = "CMl5j4uzTlP6SSSm6Z3gKHZdhyRgMNqqZCC/oJxu7fU=";
      };
      packageName = "jpl-queues";
      asdFilesToKeep = [ "jpl-queues.asd" ];
    };
    calispel = lispPackages.buildLispPackage {
      baseName = "calispel";
      version = "2009-10-19";
      description = "calispel";
      deps = [
        quicklispPackages.bordeaux-threads
        eager-future2
        jpl-queues
        jpl-util
      ];
      buildSystems = [ "calispel" ];
      src = final.fetchFromGitHub {
        owner = "hawkir";
        repo = "calispel";
        rev = "e9f2f9c1af97f4d7bb4c8ac25fb2a8f3e8fada7a";
        sha256 = "08bmf3pi7n5hadpmqqkg65cxcj6kbvm997wcs1f53ml1nb79d9z8";
        # date = 2014-10-27T17:47:36+00:00;
      };
      packageName = "calispel";
      asdFilesToKeep = [ "calispel.asd" ];
    };

    nyxtPkgs.nyxt = (lib.foldl (lib.flip deepOverride) nyxt' [
      log4cl' iterate'
    ]).overrideAttrs (drv: {
      propagatedBuildInputs = drv.propagatedBuildInputs ++ [
        calispel
      ];
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
      echo 'exec -a nyxt ${nyxtPkgs.nyxt}/bin/nyxt-lisp-launcher.sh "$@"' >> $out/bin/nyxt-repl
      chmod a+x $out/bin/nyxt-repl
      ln -s ${nyxtPkgs.nyxt} $out/src
    '';
    version = inputs.nyxt.lastModifiedDate;
  });

  nyxtCCL = final.nyxtFor final.ccl;
  nyxtSBCL = final.nyxtFor final.sbcl;

  nyxt = variants.sbcl;
  nyxt-live = final.writeScriptBin "nyxt-live" ''
    #!${final.execline}/bin/execlineb -S0
    ${variants.ccl}/bin/nyxt-repl -e "(asdf:load-system :nyxt/gtk)" -e "(nyxt:start)" $@
  '';
}
