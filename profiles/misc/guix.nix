{ config, pkgs, lib, ...}:

let
  guixBuildGroup = "guixbuild";

  guixBuildUser = id: {
    name = "guix-build-user-${toString id}";
    createHome = false;
    description = "Guix build user ${toString id}";
    extraGroups = [ guixBuildGroup "kvm" ];
    isSystemUser = true;
  };

  guixBuildUsers = numberOfUsers:
    builtins.listToAttrs
      (map (user: {
        name = user.name;
        value = user;
      }) (builtins.genList guixBuildUser numberOfUsers));
in {
    users.users = guixBuildUsers 10;
    users.groups = { "${guixBuildGroup}" = {}; };
  }
