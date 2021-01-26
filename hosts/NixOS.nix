{
  ### root password is empty by default ###
  imports = [ ../users/nixos.nix ../users/root.nix ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;
}
