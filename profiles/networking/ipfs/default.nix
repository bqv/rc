{ config, pkgs, ... }:

{
  environment.systemPackages = let
    toipfs = pkgs.writeScriptBin "toipfs" ''
      #!${pkgs.execline}/bin/execlineb -S1
      export EXECLINE_STRICT 2
      backtick -n -I dir { dirname $1 }
      backtick -n -I file { basename $1 }
      importas -i dir dir
      cd $dir
      importas -i file file
      if { touch .ipfs.$file }
      if {
        backtick -n -I hash {
          pipeline { ipfs add -qr $file } tail -n 1
        }
        importas -i hash hash
        if -n {
          ln -sf /ipfs/$hash .ipfs.$file
        }
        foreground {
          ln -sf /ipfs/$hash /tmp/ipfs.$file
        }
        echo Saved as /tmp/ipfs.$file
      }
      mv .ipfs.$file $file
    '';
  in [ toipfs ];

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
    };
    gatewayAddress = "/ip4/127.0.0.1/tcp/4501";
  };

  systemd.services.ipfs = builtins.trace "ipfs config permissions still broken" {
    serviceConfig.ExecStartPost = "${pkgs.coreutils}/bin/chmod g+r /var/lib/ipfs/config";
  };
}
