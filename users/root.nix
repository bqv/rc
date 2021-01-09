{ usr, ... }:

{
  users.users.root = {
  } // usr.secrets.root.password;

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
