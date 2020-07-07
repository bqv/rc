final: prev: rec {
  gccEmacs = final.emacsGcc.overrideAttrs (old: rec {
    name = "${old.name}-${version}";
    version = prev.lib.substring 0 7 src.rev;
    src = prev.fetchFromGitHub {
      owner = "bqv";
      repo = "emacs";
      rev = "0f468a2f8bd6b8950be92431905b79f4d36ef8fd";
      sha256 = "09hkn99jdwa0wm3kl1lfdirby22pyd5qa42hqamfd5x70s67141v";
    };
    buildInputs = old.buildInputs ++ [ prev.cairo ];
    configureFlags = old.configureFlags ++ [ "--with-pgtk" "--with-cairo" "--with-modules" ];
  });

  gccEmacsPackages = prev.lib.dontRecurseIntoAttrs (final.emacsPackagesFor gccEmacs);

  # Overridden for exwm
  emacsWithPackages = gccEmacsPackages.emacsWithPackages;
}
