inputs@{ lisp, nyxt, ... }: final: prev: let
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

    trivialBuild = args@{ baseName, ... }: lispPackages.buildLispPackage ({
      inherit baseName;
      description = baseName;
      packageName = baseName;
      buildSystems = [ baseName ];
      asdFilesToKeep = [ "${baseName}.asd" ];
    } // args);

    iterate' = quicklispPackages.iterate.overrideAttrs (drv: {
      nativeBuildInputs = [ quicklispPackages.rt ];
    });
    cl-syslog' = trivialBuild rec {
      baseName = "cl-syslog";
      version = "2006-11-28";
      deps = [ quicklispPackages.cffi ];
      src = final.fetchFromGitHub {
        owner = "lhope";
        repo = "cl-syslog";
        rev = "f62d524";
        sha256 = "rH0zOgXR3BixtVaqcYPhIfZBjqYgWnqrORQfr1mBqFM=";
      };
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

    eager-future2 = trivialBuild rec {
      baseName = "eager-future2";
      version = "2011-02-07";
      deps = [
        quicklispPackages.bordeaux-threads
        quicklispPackages.trivial-garbage
      ];
      src = final.fetchzip {
        url = "https://common-lisp.net/project/eager-future/release/eager-future2-0.2.tgz";
        sha256 = "lkLFUkTgWoOd9AMLJkqpYqP1pgpSyO4xJ4ueFm3PuFE=";
      };
    };
    jpl-util = trivialBuild rec {
      baseName = "jpl-util";
      version = "2011-06-14";
      deps = [];
      src = final.fetchFromGitHub {
        owner = "hawkir";
        repo = "cl-jpl-util";
        rev = "0311ed374e19a49d43318064d729fe3abd9a3b62";
        sha256 = "0nc0rk9n8grkg3045xsw34whmcmddn2sfrxki4268g7kpgz0d2yz";
        # date = 2015-10-05T11:08:40-07:00;
      };
    };
    jpl-queues = trivialBuild rec {
      baseName = "jpl-queues";
      version = "2009-10-19";
      deps = [
        quicklispPackages.bordeaux-threads
        jpl-util
      ];
      src = final.fetchzip {
        url = "http://www.thoughtcrime.us/software/jpl-queues/jpl-queues-0.1.tar.gz";
        sha256 = "CMl5j4uzTlP6SSSm6Z3gKHZdhyRgMNqqZCC/oJxu7fU=";
      };
    };
    calispel = trivialBuild rec {
      baseName = "calispel";
      version = "2009-10-19";
      deps = [
        quicklispPackages.bordeaux-threads
        eager-future2
        jpl-queues
        jpl-util
      ];
      src = final.fetchFromGitHub {
        owner = "hawkir";
        repo = "calispel";
        rev = "e9f2f9c1af97f4d7bb4c8ac25fb2a8f3e8fada7a";
        sha256 = "08bmf3pi7n5hadpmqqkg65cxcj6kbvm997wcs1f53ml1nb79d9z8";
        # date = 2014-10-27T17:47:36+00:00;
      };
    };

    slynk = trivialBuild rec {
      baseName = "slynk";
      deps = [
        quicklispPackages.bordeaux-threads
      ];
      inherit (final.emacsPackages.sly) src version;
      overrides = p: {
        postUnpack = "src=$src/slynk";
      };
    };

    nyxtPkgs.nyxt = (lib.foldl (lib.flip deepOverride) nyxt' [
      log4cl' iterate'
    ]).overrideAttrs (drv: rec {
      qlSetup = "${lispPackages.quicklisp}/lib/common-lisp/quicklisp/quicklisp/setup.lisp";
      propagatedBuildInputs = drv.propagatedBuildInputs ++ [
        lispPackages.quicklisp
        calispel
        slynk
       #slynk-quicklisp
       #slynk-asdf
       #slynk-named-readtables
       #slynk-macrostep
      ];
      postInstall = lib.replaceStrings [
        "sb-alien::*shared-objects*"
        "\"\""
        "='"
      ] [
        "(progn
           ;(cl:load \"${qlSetup}\" :verbose t :print t)
           (setf cffi:*foreign-library-directories*
             (cffi::explode-path-environment-variable
               \\\"NIX_LISP_LD_LIBRARY_PATH\\\")))"
        "\"cffi slynk\""
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
