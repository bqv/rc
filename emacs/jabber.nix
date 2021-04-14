{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.jabber = {
    demand = true;
    package = epkgs: if true then (builtins.trace "emacs.jabber: build fails" pkgs.emacs.pkgs.jabber) else (epkgs.trivialBuild rec {
      pname = "jabber";
      version = lib.substring 0 7 (src.rev or src.outputHash);
      src = pkgs.fetchzip {
        url = "https://gitlab.com/cnngimenez/emacs-jabber/-/archive/lexical-binding/emacs-jabber-lexical-binding.tar.gz";
        sha256 = "Fnrk3DIbbJUx+y4kwoJU0DSBvVBmMghk8dEZtLTGths=";
      };
      buildInputs = with epkgs; [
        srv fsm literate-elisp
      ];
    });
    config = ''
      nil
    '';
  };
}
