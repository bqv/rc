{ config, pkgs, ... }:

{
  services.taskserver = {
    enable = true;
    fqdn = "todo.fron.io";
    listenHost = "::";
    organisations.alpha.users = [
      "frony" "kani"
    ];
  };
}
