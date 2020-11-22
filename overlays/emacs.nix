inputs@{...}: final: prev: rec {
  emacsPgtkGccPackages = final.lib.dontRecurseIntoAttrs (final.emacsPackagesFor final.emacsPgtkGcc);
 
  # Overridden for exwm
  emacsWithPackages = emacsPgtkGccPackages.emacsWithPackages;
}
