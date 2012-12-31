{ config, lib, usr, pkgs, flake, ... }:

{
  emacs.loader.sql = let
    src = flake.inputs.sqlcmdline;
    python = pkgs.python3.withPackages (py: [
      py.pyodbc
      py.docopt
    ]);
    sqlcmdline = pkgs.writeShellScript "sqlcmdline" ''
      exec -a sqlcmdline ${python}/bin/python ${src}/sqlcmdline.py --driver '${pkgs.unixODBCDrivers.msodbcsql17.fancyName}' $@
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
