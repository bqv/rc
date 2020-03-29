{
  users.users.root = {
  } // import ../secrets/root.password.nix;

  home-manager.users.root = {
    imports = [
      ./shells/fish
      ./utilities/htop
    ];

    programs.home-manager.enable = true;
    programs.fish.enable = true;
    programs.htop.enable = true;
  };
}
