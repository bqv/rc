{ config, pkgs, ... }:

{
  environment.unixODBCDrivers = with pkgs.unixODBCDrivers; [
    msodbcsql17
  ];
  environment.systemPackages = with pkgs; [
    unixODBC
  ];
}
