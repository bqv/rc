{ config, lib, pkgs, usr, inputs, hosts, ... }:

let
  cfg = config.services.ipfs;

  clusterSecrets = usr.secrets.ipfs.cluster;

  mfs-replace-root = pkgs.buildGoModule {
    name = "mfs-replace-root";
    src = pkgs.fetchgit {
      url = "https://github.com/hsanjuan/mfs-replace-root";
      rev = "5502708ff19beaf9fc737a0b5ac30317145315f9";
      sha256 = "07vsyg22djsapxl506y0d9n35br909238zrgwgyykq8dgrb34x0c";
      # date = 2021-01-19T14:50:31+01:00;
    };
    vendorSha256 = "ZrpHFy3FVAXRfWY81VLy0kL4fX9PuNHX1cD3cbYYNqM=";
    installCheckPhase = "$out/bin/mfs-replace-root --help";
  };
in {
  environment.systemPackages = let
    # Replace a file(tree) with an ipfs symlink
    toipfs = pkgs.writeScriptBin "toipfs" ''
      #!${pkgs.execline}/bin/execlineb -WS1
      backtick -n -I dir { dirname $1 }
      backtick -n -I file { basename $1 }
      importas -i dir dir
      cd $dir
      importas -i file file
      if { touch .ipfs.$file }
      if { rm .ipfs.$file }
      backtick -n -i hash {
        pipeline { ${pkgs.ipfs}/bin/ipfs add -qr $file }
        tail -n 1
      }
      importas -i hash hash
      if { touch ${cfg.ipfsMountDir}/$hash }
      if -n {
        if { rm -rf $file } ln -sf ${cfg.ipfsMountDir}/$hash $file
      }
      foreground { ln -sf ${cfg.ipfsMountDir}/$hash /tmp/toipfs.$file }
      foreground { echo Saved as /tmp/toipfs.$file }
      exit 1
    '';

    # Abuse CoW to realise an ipfs symlink
    fromipfs = pkgs.writeScriptBin "fromipfs" ''
      #!${pkgs.execline}/bin/execlineb -WS1
      backtick -n -i hash {
        pipeline { realpath $1 }
        xargs basename
      }
      importas -i hash hash
      if { touch ${cfg.ipfsMountDir}/$hash }
      if { rm -f $1 }
      cp -Lr --reflink=auto ${cfg.ipfsMountDir}/$hash $1
    '';

    # Push a closure to ipfs using nix-ipfs
    nix-ipfs-push = pkgs.writeScriptBin "nix-ipfs-push" ''
      #!${pkgs.bash}/bin/bash

      # Build the target derivation in the public store
      nix build "$@" --no-link
      DERIVATION=$(nix eval "$@".outPath --raw)

      # Abuse nix to build CA derivations and publish them
      nix eval --no-sandbox --expr 'builtins.readFile (builtins.derivation {
        system = "${pkgs.system}";
        preferLocalBuild = true; allowSubstitutes = false;
        name = "_"; builder = "${pkgs.writeShellScript "_" ''
          echo input: $@
          export PATH=${pkgs.jq}/bin:${pkgs.coreutils}/bin:$PATH
          cd /tmp
          export HOME=$PWD
          export TERM=dumb

          # Build the CA drvs in a private store
          mkdir -p $PWD/nix
          export NIX_REMOTE=local?real=$PWD/nix/store\&store=/nix/store
          export NIX_STATE_DIR=$PWD/nix/var
          export NIX_LOG_DIR=$PWD/nix/var/log

          # Publish and output toplevel pathinfo json
          drv=$(${pkgs.nix-ipfs}/bin/nix make-content-addressable --ipfs -r $@ --json | jq '.[] | last(.[])' -r)
          ${pkgs.nix-ipfs}/bin/nix copy $drv --to ipfs:// -v
          ${pkgs.nix-ipfs}/bin/nix build $drv --substituters ipfs:// -v
          ${pkgs.nix-ipfs}/bin/nix path-info $drv --json | jq | tee $out

          echo 'Use `ipfs dag get '$(jq '.[] | .ca | split(":")[-1]' $out)' | jq` to explore'
        ''}";
        args = [ "'$DERIVATION'" ];
      })' --raw
    '';
  in [ pkgs.ipfs-cluster pkgs.brig toipfs fromipfs nix-ipfs-push mfs-replace-root ];

  services.ipfs = {
    enable = true;
    startWhenNeeded = true;
    enableGC = false;
    emptyRepo = true;
    autoMount = true;
    extraConfig = {
      DataStore = {
        StorageMax = "512GB";
      };
      Discovery = {
        MDNS.Enabled = true;
       #Swarm.AddrFilters = null;
      };
      Experimental.FilestoreEnabled = true;
      Experimental.Libp2pStreamMounting = true;
      Reprovider.Interval = "1h";
      Swarm.ConnMgr.GracePeriod = "60s";
      Swarm.EnableAutoRelay = true;
    };
    extraFlags = [ "--enable-pubsub-experiment" ];
    serviceFdlimit = 999999;
    apiAddress = "/ip4/0.0.0.0/tcp/5001";
    gatewayAddress = "/ip4/0.0.0.0/tcp/4501";
  };

  services.ipfs-cluster = {
    enable = true;
    identity = clusterSecrets.${config.networking.hostName};
    settings = {
      cluster.secret = clusterSecrets.secret;
      cluster.peer_addresses = let
        inherit (usr.secrets.ipfs.repo) proxyPeerID;
      in [
        "/ip4/${hosts.wireguard.delta}/tcp/9096/p2p/${proxyPeerID.delta}"
        "/ip4/${hosts.wireguard.zeta }/tcp/9096/p2p/${proxyPeerID.zeta }"
      ];
    };
  };

  systemd.services.ipfs-init.serviceConfig.TimeoutStartSec = "20s";
  systemd.services.ipfs-init.serviceConfig.ExecStartPre = ''
    ${mfs-replace-root}/bin/mfs-replace-root QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn
  '';

  systemd.services.ipfs = builtins.trace "${config.networking.hostName} - ipfs config permissions still broken" {
    serviceConfig.ExecStartPost = "${pkgs.coreutils}/bin/chmod g+r /var/lib/ipfs/config";
    bindsTo = ["ipfs-init.service"];
  };

  security.wrappers.ipfs = {
    source = "${pkgs.ipfs}/bin/ipfs";
    owner = config.services.ipfs.user;
    group = config.services.ipfs.group;
    setuid = true;
  };
}
