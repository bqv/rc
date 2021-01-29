{ config, lib, usr, pkgs, hosts, ... }:

let
  wanInterface = "eno1";
  vlanInterface = idx: "fo${toString idx}";
in {
  imports = [
    ../../containers/sandbox.nix   # 10. 1.0.x
    ../../containers/secure.nix    # 10. 2.0.x
   #../../containers/certmon.nix   # 10. 3.0.x
    ../../containers/authority.nix # 10. 4.0.x
    ../../containers/search.nix    # 10. 5.0.x
    ../../containers/mastodon.nix  # 10. 6.0.x
    ../../containers/matrix.nix    # 10. 7.0.x
    ../../containers/hydroxide.nix # 10. 8.0.x
    ../../containers/anki.nix      # 10. 9.0.x
    ../../containers/klaus.nix     # 10.10.0.x
    ../../containers/jellyfin.nix  # 10.11.0.x
  ];

  isolation = {
    makeHostAddress = { id, ... }: "10.${toString id}.0.1";
    makeHostAddress6 = { id, ... }: "${hosts.ipv6.zeta.prefix}:${toString id}:1";
    makeLocalAddress = { id, ... }: "10.${toString id}.0.2";
    makeLocalAddress6 = { id, ... }: "${hosts.ipv6.zeta.prefix}:${toString id}:2";
    scopes.klaus.id = 10;
  };

  networking.interfaces.${wanInterface} = {
    ipv4.addresses = [
      hosts.ipv4.zeta.address
    ];
    ipv6.addresses = let
      addrs = {
        ${hosts.ipv6.zeta.prefix} = hosts.ipv6.zeta;
      } // hosts.ipv6.zeta.subnets;
    in
      lib.mapAttrsToList (address: { length, ... }: {
        inherit address;
        prefixLength = length;
      }) addrs;
    ipv6.routes = [
      hosts.ipv6.r-zeta
    ];
  };
  networking.vlans.${vlanInterface 1} = {
    id = 0;
    interface = wanInterface;
  };
  networking.interfaces.${vlanInterface 1} = {
    ipv4.addresses = [
      hosts.ipv4.zeta-alt
    ];
  };

  networking.nat.enable = true;
  networking.nat.internalInterfaces = ["ve-+"];
  networking.nat.externalInterface = wanInterface;
  systemd.services.nat.path = with pkgs; lib.mkForce [
    iptables-nftables-compat coreutils
  ];

  networking.firewall.enable = false;
  networking.nftables = let
    inherit (usr) dag;
  in {
    enable = true;

    rules = {
      inet.filter.input = {
        wireguard-ip = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["ssh" "default"] {
          protocol = "ip"; field = "saddr";
          value = "${config.isolation.makeHostAddress 0}/24";
          policy = "accept";
        };
        wireguard = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "tcp"; field = "dport";
          value = 51820;
          policy = "accept";
        };
        http = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "tcp"; field = "dport";
          value = [ 80 443 ];
          policy = "accept";
        };
        imap = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping" "wireguard-ip"] ["default"] {
          protocol = "tcp"; field = "dport";
          value = [ 1143 ];
          policy = "drop";
        };
        smtp = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping" "wireguard-ip"] ["default"] {
          protocol = "tcp"; field = "dport";
          value = [ 1025 ];
          policy = "drop";
        };
        ipfs-api-tcp = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "tcp"; field = "dport";
          value = [ 4001 5001 ];
          policy = "accept";
        };
        ipfs-api-udp = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "udp"; field = "dport";
          value = [ 4001 5001 ];
          policy = "accept";
        };
        udpports = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "udp"; field = "dport";
          value = map (x: x+60000) (lib.genList (x: x+1) (65535-60000));
          # mosh: 60000-65535
          policy = "accept";
        };
        basic-tcp = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "tcp"; field = "dport";
          value = [
            4004# construct
            5432# postgres
            8090# yacy
            8448# synapse
            25565# minecraft
          ];
          policy = "accept";
        };
        basic-udp = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "udp"; field = "dport";
          value = [
            5353# mdns
            21027# syncthing
            25565# minecraft
          ];
          policy = "accept";
        };
      };
    };
  };

  networking.wireguard.interfaces.wg0 = {
    postSetup = let
      wanInterface = vlanInterface 1;
      ipnat = "${pkgs.iptables}/bin/iptables -w -t nat";
      proto = proto: "-p ${proto} -m ${proto}";
      icmp-echo = "--icmp-type 8";
      from-failover = "-d ${hosts.ipv4.zeta-alt}";
      to-delta = "--to-destination ${hosts.wireguard.delta}";
      lanInterface = "wg0";
    in ''
      # Enable packet forwarding to/from the target for established/related connections
     #iptables -A FORWARD -i ${wanInterface} -o ${lanInterface} -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
     #iptables -A FORWARD -i ${lanInterface} -o ${wanInterface} -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT

      # Enable masquerade on the target
     #${ipnat} -A nixos-nat-post -o ${lanInterface} -s ${hosts.wireguard.delta} -j MASQUERADE

      # Forward from source to target
     #${ipnat} -A nixos-nat-pre  -i ${wanInterface} ${proto "tcp" } ${from-failover} -j DNAT ${to-delta}

      # Hmm.
     #${ipnat} -A nixos-nat-pre  -i ${wanInterface} ${proto "icmp"} ${from-failover} -j DNAT ${to-delta} ${icmp-echo}
     #${ipnat} -A nixos-nat-pre  -i ${wanInterface} ${proto "udp" } ${from-failover} -j DNAT ${to-delta}
    '';
  };

  networking.defaultGateway = hosts.ipv4.r-zeta;
  networking.nameservers = [ "9.9.9.9" ];

  environment.etc.dhclient6 = {
    target = "dhcp/dhclient6.conf";
    text = ''
      interface "${wanInterface}" {
         send dhcp6.client-id ${hosts.ipv6.zeta.duid};
      }
    '';
  };

  networking.enableIPv6 = true;
  systemd.services.dhclient = {
    description = "Client for sending IPv6 DUID";
    wants = [ "network-link-${wanInterface}.service" ];
    after = [ "network-link-${wanInterface}.service" ];
    before = [ "network-addresses-${wanInterface}.service" ];
    serviceConfig = {
      Type = "forking";
    };
    script = with config.environment; "${pkgs.dhcp}/sbin/dhclient -cf /etc/${etc.dhclient6.target} -6 -P -v ${wanInterface}";
    wantedBy = [ "network.target" ];
  };
  systemd.services."network-addresses-${wanInterface}" = {
    after = [ "dhclient.service" ];
    partOf = [ "dhclient.service" ];
  };
}
