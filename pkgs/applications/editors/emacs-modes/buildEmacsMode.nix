{ stdenv, ... }:

drv@{name, src}: stdenv.mkDerivation (
  drv // {
    installPhase = ''
      mkdir -p $out/share/emacs/site-lisp
      cp *.el *.elc $out/share/emacs/site-lisp/
    '';
  }
)
