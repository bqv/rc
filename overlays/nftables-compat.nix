final: prev: {
  iptables = prev.iptables-nftables-compat;
  systemd = prev.systemd.override { iptables = prev.iptables; };
}
