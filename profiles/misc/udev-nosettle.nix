{ pkgs, ... }: {
  # Disable `systemd-udev-settle` - it's required and just adds 1s to boot time.
  # See nixpkgs#25311.
  systemd.services.systemd-udev-settle.serviceConfig.ExecStart = [
    ""
    "${pkgs.coreutils}/bin/true"
  ];
}
