{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.sql = let
    src = pkgs.fetchFromGitHub {
      owner = "sebasmonia";
      repo = "sqlcmdline";
      rev = "1d4e97773f76f94875c4e4393abd3c8e7b6ec06d";
      sha256 = "0p0byvl0h61yfy5gqj7d667axssnmxp0lgqbwwv1dyx2ryy6x8yi";
      # date = 2020-03-09T08:41:02-06:00;
    };
    python = pkgs.python3.withPackages (py: [
      py.pyodbc
      py.docopt
    ]);
    sqlcmdline = pkgs.writeShellScript "sqlcmdline" ''
      #!${pkgs.execline}/bin/execlineb -S0
      exec -a sqlcmdline
      ${python}/bin/python ${src}/sqlcmdline.py
    '';
  in {
    demand = true;
    package = lib.const null;
    config = ''
      (with-eval-after-load 'sql-mssql
        (plist-put (alist-get 'ms sql-product-alist) :prompt-cont-regexp "^[0-9]*>")

        (setq sql-ms-options nil)
        (setq sql-ms-programs "${sqlcmdline}"))
    '';
  };
}
