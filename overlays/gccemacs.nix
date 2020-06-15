final: prev: rec {
  gccEmacs = final.emacsGcc.overrideAttrs (o: {
    configureFlags = o.configureFlags ++ [ "--with-json" ];
  });

  gccEmacsPackages = prev.lib.dontRecurseIntoAttrs (final.emacsPackagesFor gccEmacs);

  # Overridden for exwm
  emacsWithPackages = gccEmacsPackages.emacsWithPackages;
}
