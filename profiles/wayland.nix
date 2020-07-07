{ config, pkgs, ... }:

{
  imports = [
    ./services/wayland/swc-launch.nix
  ];

  environment.systemPackages = with pkgs; with velox; [
    dmenu
    dmenu-velox
    st-velox
    xwayland
  ];

  environment.etc = {
    "velox.conf".source = "${pkgs.velox}/etc/velox.conf";
  };

  # Enable swc+velox (Wayland compositor) as alternative to X11
  services.swc-launch = {
    enable = true;
    user = "bao";
    layout = "gb";
    xkbOptions = "caps:ctrl_modifier";
    server.velox.enable = true;
  };
}
