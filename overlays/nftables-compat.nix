final: prev: rec {
  iptables = prev.iptables-nftables-compat;
  systemd = prev.systemd.override { iptables = prev.iptables; };
  networkmanager = prev.networkmanager.override {
    dhcp = prev.dhcp.override {
      iproute = prev.iproute.override {
        iptables = prev.iptables;
      };
    };
    iptables = prev.iptables;
  };
}
