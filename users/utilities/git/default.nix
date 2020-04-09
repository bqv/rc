{ config ? {}, lib, pkgs, ... }:

with lib; let
  cfg = config.programs.git;
in {
  config = mkIf cfg.enable {
    home.packages = with pkgs; let
      git-crypt = pkgs.git-crypt.overrideAttrs (attrs: rec {
        worktreePatch = fetchurl {
          url = "https://github.com/AGWA/git-crypt/files/2771938/git-crypt-support-worktree-simple-version-patch.txt";
          sha256 = "1k477m6g3zjdarjr38lndh0kpgkp0yi8lg2iqdispfd4c85krrax";
        };
        patches = [ worktreePatch ];
      });
    in [
      dgit
      git
      git-crypt
      gitAndTools.hub
      gitAndTools.lab
    ];
  };
}
