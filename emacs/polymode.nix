{ config, lib, usr, pkgs, ... }:

let
  undash = lib.strings.replaceStrings ["-"] [""];

  basicPolymode = { host, inner, discrim, headPat, tailPat }: let
    hostmode = "poly-${undash host}-hostmode";
    innermode = "poly-${undash host}-${discrim}-${undash inner}-innermode";
    polymode = "${undash host}-${discrim}-${undash inner}-mode";
  in ''
    (define-hostmode ${hostmode}
      :mode '${host}-mode)

    (define-innermode ${innermode}
      :mode '${inner}-mode
      :head-matcher "${headPat}"
      :tail-matcher "${tailPat}"
      :head-mode 'host
      :tail-mode 'host)

    (define-polymode ${polymode}
      :hostmode '${hostmode}
      :innermodes '(${innermode}))
  '';
in {
  emacs-loader.polymode = { config, ... }: {
    options.polymodes = lib.mkOption {
      type = with lib.types; loaOf (lines);
      default = [];
    };

    config = {
      demand = true;
      polymodes = {
        nix-elisp = basicPolymode {
          host = "nix";
          inner = "emacs-lisp";
          discrim = "dsquoted";
          headPat = "''\\n";
          tailPat = "^\\s*''";
        };
        nix-clisp = basicPolymode {
          host = "nix";
          inner = "common-lisp";
          discrim = "dsquoted";
          headPat = "''\\n";
          tailPat = "^\\s*''";
        };
        nix-sh = basicPolymode {
          host = "nix";
          inner = "sh";
          discrim = "dsquoted";
          headPat = "''\\n";
          tailPat = "^\\s*''";
        };
      };
      config = (lib.concatStrings (builtins.attrValues config.polymodes)) + ''

      '';
    };
  };
}

## Local Variables: ***
## mode: nix-dsquoted-emacslisp ***
## End: ***
