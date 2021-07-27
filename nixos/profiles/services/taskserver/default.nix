{ config, pkgs, domains, ... }:

{
  services.taskserver = {
    enable = true;
    fqdn = "todo.${domains.home}";
    listenHost = "::";
    organisations.alpha.users = [
      "frony" "kani"
    ];
  };
}
