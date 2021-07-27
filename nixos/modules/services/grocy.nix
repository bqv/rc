{ config, lib, domains, ... }:

{
  config = lib.mkIf config.services.grocy.enable {
    services.grocy = {
      nginx.enableSSL = false;
      hostName = "grocy.${domains.home}";
      settings = {
        currency = "GBP";
        culture = "en_GB";
        calendar.firstDayOfWeek = 1;
        calendar.showWeekNumber = true;
      };
    };
    services.nginx.virtualHosts.${config.services.grocy.hostName}.listen = [{
      addr = "[::]";
      port = 8789;
      ssl = false;
    }{
      addr = "0.0.0.0";
      port = 8789;
      ssl = false;
    }];
  };
}
