{ config, lib, pkgs, ... }:

with lib;

let
  random-ipv6-script = pkgs.writeScript "ramdom-ipv6.py" ''
    #!${pkgs.python3}/bin/python
    # https://blog.fugoes.xyz/2018/02/03/Run-Babeld-over-Wireguard.html
    import random

    def random_mac():
        digits = [0x00, 0x16, 0x3e, random.randint(0x00, 0x7f), random.randint(0x00, 0xff), random.randint(0x00, 0xff)]
        return ":".join(map(lambda x: "%02x" % x, digits))

    def mac_to_ipv6(mac):
        parts = mac.split(":")
        parts.insert(3, "ff")
        parts.insert(4, "fe")
        parts[0] = "%x" % (int(parts[0], 16) ^ 2)
        ipv6_parts = []
        for i in range(0, len(parts), 2):
            ipv6_parts.append("".join(parts[i:i + 2]))
        return "fe80::%s/64" % (":".join(ipv6_parts))

    def random_ipv6():
        return mac_to_ipv6(random_mac())

    if __name__ == "__main__":
        print(random_ipv6(), end="")
  '';
  # runCommandNoCC name: env: buildCommand:
  random-ipv6 = name: builtins.readFile (toString (pkgs.runCommandNoCC "ipv6-${name}" {} ''
    mkdir $out
    ${random-ipv6-script} > $out/ipv6
    '')+"/ipv6");

  cfg = config.networking.wireguard-mesh;

  peerNames = builtins.filter (n: n != config.networking.hostName) (builtins.attrNames cfg.peers);
in {
  options = {
    networking.wireguard-mesh = {
      enable = mkEnableOption "Enable a wireguard mesh network";
      ipv4Address = mkOption {
        type = types.str;
      };
      privateKeyFile = mkOption {
        default = toString "/secrets/wireguard_key";
      };
      peers = mkOption {
        default = {};
        #type = with types; attrsOf (submodule peerOpts);
        example = {
        };
        description = ''
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    # https://www.sweharris.org/post/2016-10-30-ssh-certs/
    # http://www.lorier.net/docs/ssh-ca
    # https://linux-audit.com/granting-temporary-access-to-servers-using-signed-ssh-keys/
    # rpi31
    networking.wireguard.interfaces = listToAttrs (flip map peerNames
      (n : let
        peer = builtins.getAttr n cfg.peers;
        in nameValuePair "${n}" {
        ips = [
          cfg.peers."${config.networking.hostName}".ipv4Address
          "${random-ipv6 "${config.networking.hostName}-${n}"}"
        ];
        listenPort = peer.listenPort;
        allowedIPsAsRoutes=false;
        privateKeyFile = cfg.privateKeyFile;
        peers = [
          { allowedIPs = [ "0.0.0.0/0" "ff02::/16" "::/0" ];
            publicKey  = peer.publicKey;
            endpoint   = mkIf (peer ? endpoint) peer.endpoint;
            persistentKeepalive = mkIf (peer ? persistentKeepalive) peer.persistentKeepalive;
          }
        ];
      }));

    boot.kernel.sysctl = {
      "net.ipv4.ip_forward" = "1";
      "net.ipv6.conf.all.forwarding"="1";
    };
    services.babeld.enable = true;
    services.babeld.interfaceDefaults = {
      type = "tunnel";
      "split-horizon" = true;
    };
    services.babeld.extraConfig = ''
      ${concatMapStrings (n: ''
        interface ${n}
      '') peerNames}
      # mesh IPv4
      redistribute local ip 10.147.27.0/24 metric 128
      redistribute ip 10.147.27.0/24 ge 13 metric 128
      ## refuse anything else not explicitely allowed
      redistribute local deny
      redistribute deny
    '';

    networking.firewall.allowedUDPPorts = [ 6696 ];
  };
}
