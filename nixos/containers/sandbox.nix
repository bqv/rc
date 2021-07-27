{ config, pkgs, ... }:

{
  systemd.nspawn.sandbox = {
    aliases = [ "sandbox" ];
    execConfig = {
      Boot = true;
    };
    filesConfig = {
      BindReadOnly = [
        "/home:/var/home/lower"
        "/etc/sandbox/fstab:/etc/fstab"
        "/etc/sandbox/hostname:/etc/hostname"
      ];
      Bind = [ "/srv" ];
    };
    networkConfig = {
      VirtualEthernet = true;
    };
    wantedBy = [
      "multi-user.target"
    ];
    requiredBy = [
      "network-link-ve-sandbox.service"
    ];
  };
  systemd.units."network-link-ve-sandbox.service".requiredBy = [
    "systemd-nspawn@sandbox.service"
  ];
  networking.interfaces.ve-sandbox = {
    useDHCP = true;
    ipv4 = {
      addresses = [
        { address = "10.1.0.1"; prefixLength = 32; }
      ];
      routes = [
        { address = "10.1.0.2"; prefixLength = 32; options = { src = "10.1.0.1"; }; }
      ];
    };
  };
  environment.etc.sandbox-fstab = {
    target = "sandbox/fstab";
    text = ''
      # <file system>	<dir>		<type>	<options>									<dump>	<pass>
      tmpfs		/var/home      	tmpfs	rw,nosuid,noatime,size=512m							0	0
      overlayfs		/home      	overlay	rw,lowerdir=/var/home/lower,upperdir=/var/home/upper,workdir=/var/home/work  	0	0
      /srv/sync/Sources	/usr/local/src	none	rw,bind										0	0
    '';
  };
}
