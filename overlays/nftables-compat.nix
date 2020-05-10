final: prev: {
  iptables = prev.iptables-nftables-compat;
  systemd = prev.systemd.override { iptables = prev.iptables; };
  webkitgtk = prev.webkitgtk.override {
    geoclue2 = prev.geoclue2.override {
      glib-networking = prev.glib-networking.override {
        libproxy = prev.libproxy.override {
          networkmanager = prev.networkmanager.override {
            dhcp = prev.dhcp.override {
              iproute = prev.iproute.override {
                iptables = prev.iptables;
              };
            };
            iptables = prev.iptables;
          };
        };
      };
    };
  };
}
