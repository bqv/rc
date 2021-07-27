inputs@{...}: final: prev: {
  iptables = final.iptables-nftables-compat;
}
