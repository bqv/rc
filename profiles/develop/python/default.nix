{ pkgs, ... }:

{
  environment.systemPackages = let
    packages = pythonPackages:
      with pythonPackages; [
        nixpkgs
        numpy
        pandas
        ptpython
        requests
        scipy
        virtualenv
      ];

    python = pkgs.python3.withPackages packages;
    inherit (pkgs.python3Packages) virtualenv;
  in [ python virtualenv ];

  environment.sessionVariables = {
    PYTHONSTARTUP = let
      startup = pkgs.writers.writePython3 "ptpython.py" {
        libraries = with pkgs.python3Packages; [ ptpython ];
      } ''
        from __future__ import unicode_literals

        from pygments.token import Token

        from ptpython.layout import CompletionVisualisation

        import sys

        ${builtins.readFile ./ptconfig.py}

        try:
            from ptpython.repl import embed
        except ImportError:
            print("ptpython is not available: falling back to standard prompt")
        else:
            sys.exit(embed(globals(), locals(), configure=configure))
      '';
    in "${startup}";
  };
}
