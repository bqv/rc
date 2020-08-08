final: prev: rec {
  gccEmacs = let
    shortrev = prev.lib.substring 0 7;

    nrev = final.emacsGcc.src.rev;
    wrev = final.emacs-pgtk.src.rev;

    nativecomp = prev.fetchzip {
      url = "http://github.com/emacs-mirror/emacs/archive/${nrev}.zip";
      sha256 = final.emacsGcc.src.outputHash;
    };
    pgtk = prev.fetchzip {
      url = "http://github.com/masm11/emacs/archive/${wrev}.zip";
      sha256 = final.emacs-pgtk.src.outputHash;
    };

    version = "${shortrev nrev}+${shortrev wrev}";
    src = prev.runCommandNoCC "git-merge" { buildInputs = with prev; [ git ]; } ''
      cp ${nativecomp} src -Lr && cd src && chmod -R a+w .
      git fetch ${pgtk} && git merge FETCH_HEAD -m nixmerge
      rm -rf .git && cp -r . $out
    '';
  in final.emacsGcc.overrideAttrs (old: {
    name = "${old.name}-${version}";
    inherit src version;
    buildInputs = old.buildInputs ++ [ final.cairo ];
    configureFlags = old.configureFlags ++ [ "--with-pgtk" "--with-cairo" "--with-modules" ];
  });

  gccEmacsPackages = prev.lib.dontRecurseIntoAttrs (final.emacsPackagesFor gccEmacs);

  # Overridden for exwm
  emacsWithPackages = gccEmacsPackages.emacsWithPackages;
}
