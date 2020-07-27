{ callPackage, symlinkJoin, writeShellScriptBin, wrapLisp, clisp, sbcl }:

let
  nyxt = { pkgs,
      useQt ? false, useGtk ? !useQt,
      useClisp ? false, useSbcl ? !useClisp
    }:

    assert useGtk -> !useQt;
    assert useQt  -> !useGtk;

    assert useSbcl  -> !useClisp;
    assert useClisp -> !useSbcl;

    with pkgs; let
      lisp = wrapLisp (if useClisp then clisp else sbcl);

      lispCmd = if useClisp then "common-lisp.sh -on-error appease" else
                if useSbcl  then "common-lisp.sh" else null;

      applicationModule = if useGtk then "nyxt/gtk-application" else
                          if useQt  then "nyxt/qt-application" else null;

      evalCmd = if useSbcl  then "--eval" else
                if useClisp then "-x" else null;
    in stdenv.mkDerivation rec {
      pname = "nyxt";
      version = lib.substring 0 7 src.rev;

      src = fetchgit {
        url = "https://github.com/atlas-engineer/nyxt";
        rev = "8251d8b076e399d0e85ad75431f32ffdd452978f";
        sha256 = "lEY5qNhJayRmSjdCQuiS9COY7pVRHRwiq9iSCatdL78=";
        fetchSubmodules = true;
      };

      nativeBuildInputs = [ git cacert makeWrapper wrapGAppsHook ];
      buildInputs = [
        lisp openssl libfixposix mime-types
        glib gdk-pixbuf cairo
        pango gtk3 webkitgtk vivaldi-widevine
        glib-networking gsettings-desktop-schemas
        xclip notify-osd enchant
      ];

      configurePhase = ''
        mkdir -p quicklisp-client/local-projects
        for i in quicklisp-libraries/*; do ln -sf "$(readlink -f "$i")" "quicklisp-client/local-projects/$(basename "$i")"; done
      '';

      dontStrip = true;
      buildPhase = ''
        ${lispCmd} ${evalCmd} '(require "asdf")' \
                   ${evalCmd} '(load "quicklisp-client/setup.lisp")' \
                   ${evalCmd} '(asdf:load-asd (truename "nyxt.asd") :name "nyxt")' \
                   ${evalCmd} '(ql:quickload :${applicationModule})' \
                   ${evalCmd} '(quit)'

        ${lispCmd} ${evalCmd} '(load "quicklisp-client/setup.lisp")' \
                   ${evalCmd} '(require "asdf")' \
                   ${evalCmd} '(ql:update-dist "quicklisp" :prompt nil)' \
                   ${evalCmd} '(quit)'
      '';

      dontWrapGApps = true;
      installPhase = ''
        cp -r . $out && cd $out && export HOME=$out

        ${lispCmd} ${evalCmd} '(require "asdf")' \
                   ${evalCmd} '(load "quicklisp-client/setup.lisp")' \
                   ${evalCmd} '(asdf:load-asd (truename "nyxt.asd") :name "nyxt")' \
                   ${evalCmd} '(asdf:make :${applicationModule})' \
                   ${evalCmd} '(quit)'

        mkdir -p $out/share/applications/
        sed "s/VERSION/${version}/" assets/nyxt.desktop > $out/share/applications/nyxt.desktop
        rm -f version
        for i in 16 32 128 256 512; do \
                mkdir -p "$out/share/icons/hicolor/''${i}x''${i}/apps/" ; \
                cp -f assets/nyxt_''${i}x''${i}.png "$out/share/icons/hicolor/''${i}x''${i}/apps/nyxt.png" ; \
                done

        install -D -m0755 nyxt $out/libexec/nyxt
        mkdir -p $out/bin && makeWrapper $out/libexec/nyxt $out/bin/nyxt \
          --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath buildInputs}" \
          --argv0 nyxt "''${gappsWrapperArgs[@]}"
      '';

      checkPhase = ''
        $out/bin/nyxt -h
      '';

      meta = pkgs.next.meta // {
        broken = pkgs.system != "x86_64-linux";
      };

      __noChroot = true;
      # Packaged like a kebab in duct-tape, for now
    };
in callPackage nyxt {}
