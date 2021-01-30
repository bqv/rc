{ config, lib, pkgs, usr, hosts, ... }:

let
  cfg = config.networking.nftables;

  inherit (usr) dag;

  mkTable = desc: body: lib.mkOption {
    default = {};
    type = lib.types.submodule ({ config, ... }: {
      options = {
        enable = lib.mkEnableOption desc;
        objects = lib.mkOption {
          type = with lib.types; listOf str;
          description = "Objects associated with this table.";
        };
      } // body;

      config = let
        buildChainDag = chain: lib.concatMapStringsSep "\n" ({ name, data }: let
          protocol = if builtins.isNull data.protocol then "" else data.protocol;
          field = if builtins.isNull data.field then "" else data.field;
          inherit (data) policy;
          values = map toString data.value;
          value = if builtins.isNull data.value then "" else (
            if builtins.length data.value == 1
            then builtins.head values
            else "{ ${lib.concatStringsSep ", " values} }");
        in ''
          ${protocol} ${field} ${value} ${policy} comment ${name}
        '') ((dag.topoSort chain).result or (throw "Cycle in DAG"));
        buildChain = chainType: chain: lib.mapAttrsToList (chainName: chainDag: ''
          chain ${chainName} {
            type ${chainType} hook ${chainName} priority 0;

            ${buildChainDag chainDag}
          }
        '') (lib.filterAttrs (_: g: builtins.length (builtins.attrNames g) > 0) chain);
      in {
        objects = let
          chains = (if config ? filter then buildChain "filter" config.filter else [])
                ++ (if config ? nat then buildChain "nat" config.nat else [])
                ++ (if config ? route then buildChain "route" config.route else []);
        in chains;
      };
    });
    description = "Containers for chains, sets, and other stateful objects.";
  };

  mkChain = family: description: lib.mkOption {
    inherit description;
    default = {};
    type = dag.types.dagOf (lib.types.submodule {
      options = {
        protocol = lib.mkOption {
          default = null;
          type = with lib.types; nullOr (either (enum [
            "ether" "vlan" "arp" "ip" "icmp" "igmp" "ip6" "icmpv6"
            "tcp" "udp" "udplite" "sctp" "dccp" "ah" "esp" "comp"
          ]) str);
          description = "Protocol to match.";
        };
        field = lib.mkOption {
          default = null;
          type = with lib.types; nullOr (enum [
            "dport" "sport" "daddr" "saddr" "type" "state" "iifname"
          ]);
          description = "Value to match.";
        };
        value = lib.mkOption {
          default = null;
          type = with lib.types; let
            valueType = oneOf [ port str ];
          in nullOr (coercedTo valueType (v: [v]) (listOf valueType));
          description = "Associated value.";
        };
        policy = lib.mkOption {
          type = lib.types.enum [
            "accept" "reject" "drop" "log"
          ];
          description = "What to do with matching packets.";
        };
      };
    });
  };

  mkIngressChain = mkChain "Process all packets before they enter the system";
  mkPrerouteChain = mkChain "Process all packets entering the system";
  mkInputChain = mkChain "Process packets delivered to the local system";
  mkForwardChain = mkChain "Process packets forwarded to a different host";
  mkOutputChain = mkChain "Process packets sent by local processes";
  mkPostrouteChain = mkChain "Process all packets leaving the system";
in {
  options = {
    networking.nftables.rules = { # man nft(8)
      ip = mkTable "internet (IPv4) address family netfilter table" {
        filter.prerouting = mkPrerouteChain "ip";
        filter.input = mkInputChain "ip";
        filter.forward = mkForwardChain "ip";
        filter.output = mkOutputChain "ip";
        filter.postrouting = mkPostrouteChain "ip";
        nat.prerouting = mkPrerouteChain "ip";
        nat.input = mkInputChain "ip";
        nat.output = mkOutputChain "ip";
        nat.postrouting = mkPostrouteChain "ip";
        route.output = mkForwardChain "ip";
      };
      ip6 = mkTable "internet (IPv6) address family netfilter table" {
        filter.prerouting = mkPrerouteChain "ip6";
        filter.input = mkInputChain "ip6";
        filter.forward = mkForwardChain "ip6";
        filter.output = mkOutputChain "ip6";
        filter.postrouting = mkPostrouteChain "ip6";
        nat.prerouting = mkPrerouteChain "ip6";
        nat.input = mkInputChain "ip6";
        nat.output = mkOutputChain "ip6";
        nat.postrouting = mkPostrouteChain "ip6";
        route.output = mkForwardChain "ip6";
      };
      inet = mkTable "internet (IPv4/IPv6) address family netfilter table" {
        filter.prerouting = mkPrerouteChain "inet";
        filter.input = mkInputChain "inet";
        filter.forward = mkForwardChain "inet";
        filter.output = mkOutputChain "inet";
        filter.postrouting = mkPostrouteChain "inet";
        nat.prerouting = mkPrerouteChain "inet";
        nat.input = mkInputChain "inet";
        nat.output = mkOutputChain "inet";
        nat.postrouting = mkPostrouteChain "inet";
      };
      arp = mkTable "ARP (IPv4) address family netfilter table" {
        filter.input = mkInputChain "arp";
        filter.output = mkOutputChain "arp";
      };
      bridge = mkTable "bridge address family netfilter table" {
        filter.prerouting = mkPrerouteChain "bridge";
        filter.input = mkInputChain "bridge";
        filter.forward = mkForwardChain "bridge";
        filter.output = mkOutputChain "bridge";
        filter.postrouting = mkPostrouteChain "bridge";
      };
      netdev = mkTable "netdev address family netfilter table" {
        filter.ingress = mkIngressChain "netdev";
      };
    };
  };

  config = {
    boot.extraModprobeConfig = lib.optionalString cfg.enable ''
      install ip_tables ${pkgs.coreutils}/bin/true
    '';

    networking.firewall.enable = !cfg.enable;
    networking.firewall.package = if cfg.enable
      then pkgs.iptables-nftables-compat
      else pkgs.iptables;

    networking.nftables.enable = lib.mkDefault true;
    networking.nftables.ruleset = lib.mkDefault
      (lib.concatStringsSep "\n" (lib.mapAttrsToList (name: table:
        lib.optionalString (builtins.length table.objects > 0) ''
          table ${name} nixos {
            ${lib.concatStringsSep "\n" table.objects}
          }
        '') cfg.rules));
    networking.nftables.rules = {
      inet.filter.input = {
        loopback = dag.entryAnywhere {
          field = "iifname";
          value = "lo";
          policy = "accept";
        };
        established-locally = dag.entryAfter ["loopback"] {
          protocol = "ct";
          field = "state";
          value = [ "established" "related" ];
          policy = "accept";
        };
        basic-icmp6 = dag.entryAfter ["loopback" "established-locally"] {
          protocol = "ip6 nexthdr icmpv6 icmpv6";
          field = "type";
          value = [
            "destination-unreachable" "packet-too-big" "time-exceeded" "parameter-problem"
            "nd-router-advert" "nd-neighbor-solicit" "nd-neighbor-advert"
           #"mld-listener-query" "nd-router-solicit" # for routers
          ];
          policy = "accept";
        };
        basic-icmp = dag.entryAfter ["loopback" "established-locally"] {
          protocol = "ip protocol icmp icmp";
          field = "type";
          value = [
            "destination-unreachable" "router-advertisement" "time-exceeded" "parameter-problem"
          ];
          policy = "accept";
        };
        ping6 = dag.entryBefore ["basic-icmp6"] {
          protocol = "ip6 nexthdr icmpv6 icmpv6";
          field = "type";
          value = "echo-request";
          policy = "accept";
        };
        ping = dag.entryBefore ["basic-icmp"] {
          protocol = "ip protocol icmp icmp";
          field = "type";
          value = "echo-request";
          policy = "accept";
        };
        ssh = dag.entryBetween ["basic-icmp6" "basic-icmp" "ping6" "ping"] ["default"] {
          protocol = "tcp";
          field = "dport";
          value = 22;
          policy = "accept";
        };
        default = dag.entryAfter ["loopback" "established-locally" "basic-icmp6" "basic-icmp" "ping6" "ping"] {
          policy = lib.mkDefault "drop";
        };
      };

      inet.filter.output = {
        default = dag.entryAnywhere {
          policy = "accept";
        };
      };

      inet.filter.forward = {
        default = dag.entryAnywhere {
          policy = "accept";
        };
      };
    };

    assertions = let
      ruleset = pkgs.writeText "nft-ruleset" cfg.ruleset;
      check-results = pkgs.runCommand "check-nft-ruleset" {} ''
        mkdir -p $out
        ${pkgs.nftables}/bin/nft -c -f ${ruleset} 2>&1 > $out/message \
          && echo false > $out/assertion \
          || echo true > $out/assertion
      '';
    in [
      {
        message = ''
          Bad config:
          ${builtins.readFile "${check-results}/message"}
        '';
        assertion = import "${check-results}/assertion";
      }
    ];
  };
}
