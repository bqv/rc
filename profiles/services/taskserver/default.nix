{ config, pkgs, ... }:

{
  services.taskserver = {
    enable = true;
    fqdn = "todo.***REMOVED***";
    listenHost = "::";
    organisations.alpha.users = [
      "frony" "kani"
    ];
  };
}
