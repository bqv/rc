{ config, lib, pkgs, usr, domains, ... }:

with lib; let
  cfg = config.programs.git;
in {
  config = mkIf cfg.enable {
    home.packages = with pkgs;
      let
        git-crypt = pkgs.git-crypt.overrideAttrs (attrs: rec {
          worktreePatch = fetchurl {
            url = "https://github.com/AGWA/git-crypt/files/2771938/git-crypt-support-worktree-simple-version-patch.txt";
            sha256 = "1k477m6g3zjdarjr38lndh0kpgkp0yi8lg2iqdispfd4c85krrax";
          };
          patches = [ worktreePatch ];
        });
      in [
        git-bug git-appraise git-pr-mirror git-remote-ipfs
        hub lab git-crypt git-secrets
        git-filter-repo git-absorb git-get stagit
      ];

    programs.git = {
      inherit (usr.secrets.git.user) userName userEmail;

      package = pkgs.hiPrio pkgs.gitFull;
      aliases = lib.mkForce { }; # We hate aliases
      ignores = [ ];
      delta = {
        enable = true;
        options = {
          syntax-theme = "Monokai Extended";
          background-color-extends-to-terminal-width = false;
        };
      };
      signing = {
        gpgPath = "${pkgs.gnupg}/bin/gpg2";
        key = "9E2FF3BDEBDFC910";
        signByDefault = true;
      };

      attributes = [ ".rs filter=spacify_trim" ];
      extraConfig = {
        credential.helper = "store";

        extensions.worktreeConfig = "true";
        pull.rebase = "merges";
        push = {
          default = "simple";
          followTags = true;
        };
        rebase.abbreviateCommands = true;

        url = {
          "git://github.com/" = {
            insteadOf = "github:";
          };
          "git@github.com:" = {
            insteadOf = "gh:";
            pushInsteadOf = [ "github:" "git://github.com/" ];
          };

          "git://gitlab.com/" = {
            insteadOf = "gitlab:";
          };
          "git@gitlab.com:" = {
            insteadOf = "gl:";
            pushInsteadOf = [ "gitlab:" "git://gitlab.com/" ];
          };

          "git://gist.github.com/" = {
            insteadOf = "gist:";
          };
          "git@gist.github.com:" = {
            insteadOf = "gst:";
            pushInsteadOf = [ "gist:" "git://gist.github.com/" ];
          };
        };
        github = {
          inherit (usr.secrets.git.github) user oauth-token;
        };

        format.pretty = "oneline";
        log.decorate = "full";
        diff = {
          guitool = "gvimdiff";
          tool = "vimdiff";
        };
        merge = {
          guitool = "gvimdiff";
          tool = "vimdiff";
        };
        filter = {
          spacify = {
            clean = "expand --tabs=4 --initial";
            required = true;
          };
          spacify_trim = {
            clean = ''sh -c "expand --tabs=4 --initial %f | git-stripspace"'';
            required = true;
          };
          trim = {
            clean = "git-stripspace";
            required = true;
          };
        };

        gitget = {
          root = "/srv/git";
        };
        inherit (usr.secrets.git.github) git-bug;
      };
    };
  };
}
