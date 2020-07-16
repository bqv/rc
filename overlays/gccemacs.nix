final: prev: rec {
  gccEmacs = let
    shortrev = prev.lib.substring 0 7;
    preMerged = prev.fetchFromGitHub {
      owner = "bqv";
      repo = "emacs";
      rev = "0f468a2f8bd6b8950be92431905b79f4d36ef8fd";
      sha256 = "09hkn99jdwa0wm3kl1lfdirby22pyd5qa42hqamfd5x70s67141v";
    };
    nrev = final.emacsGcc.src.rev;
    wrev = "a9829672f70ec0c6cd9d7e6b09ba762ea22e86b5";

    nativecomp = prev.fetchzip {
      url = "http://github.com/emacs-mirror/emacs/archive/${nrev}.zip";
      sha256 = "sha256-3jLpUATsIupns9uTLDiRI84vX21Crps/TQquhUOMUkQ=";
    };
    pgtk = prev.fetchzip {
      url = "http://github.com/masm11/emacs/archive/${wrev}.zip";
      sha256 = "sha256-BLatD6eIraYbRtNNzi9HVwsF8Z5hmGy/1E8bKtNZrUg=";
    };

    version = "${shortrev nrev}+${shortrev wrev}";
    src = prev.runCommandNoCC "git-merge" { buildInputs = with prev; [ git ]; } ''
      cp ${pgtk} src -Lr && cd src && chmod -R a+w .
      git fetch ${nativecomp} && git merge FETCH_HEAD -m nixmerge
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
