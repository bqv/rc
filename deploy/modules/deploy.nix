{ nixus, lib, config, ... }:

let
  inherit (lib) types;

  nodeOptions = node@{ name, pkgs, config, ... }: let
    switchScript = pkgs.runCommandNoCC "switch" {
      inherit (config) switchTimeout successTimeout ignoreFailingSystemdUnits privilegeEscalationCommand systemSwitcherDir;
      inherit (nixus.pkgs) execline;
    } ''
      mkdir -p $out/bin
      substituteAll ${../scripts/switch} $out/bin/switch
      chmod +x $out/bin/switch
    '';
  in {
    options = {
      deployScriptPhases = lib.mkOption {
        type = types.dagOf types.lines;
        default = {};
      };

      deployScriptPath = lib.mkOption {
        type = types.listOf types.package;
        default = (with nixus.pkgs; [
          # Without bash being here deployments to localhost do not work. The
          # reason for that is not yet known. Reported in #6.
          bash
          coreutils
          execline
          findutils
          gnused
          jq
          openssh
          procps
          rsync
        ]);
      };

      nodeDeployScript = lib.mkOption {
        type = types.package;
      };

      successTimeout = lib.mkOption {
        type = types.ints.unsigned;
        default = 20;
        description = ''
          How many seconds remote hosts should wait for the success
          confirmation before rolling back.
        '';
      };

      switchTimeout = lib.mkOption {
        type = types.ints.unsigned;
        default = 60;
        description = ''
          How many seconds remote hosts should wait for the system activation
          command to finish before considering it failed.
        '';
      };

      ignoreFailingSystemdUnits = lib.mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether a system activation should be considered successful despite
          failing systemd units.
        '';
      };

      # TODO: What about different ssh ports? Some access abstraction perhaps?
      host = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        example = "root@172.18.67.46";
        description = ''
          How to reach the host via ssh. Deploying is disabled if null. The
          username must either be root, or a user that is allowed to do
          passwordless privilege escalation. If no username is given, the one
          that runs the deploy script is used.
        '';
      };

      hasFastConnection = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Whether there is a fast connection to this host. If true it will cause
          all derivations to be copied directly from the deployment host. If
          false, the substituters are used when possible instead.
        '';
      };

      closurePaths = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [];
        description = ''
          Derivation paths to copy to the host while deploying
        '';
      };

      systemSwitcherDir = lib.mkOption {
        type = lib.types.str;
        default = "/var/lib/system-switcher/";
        description = ''
          Path that will contain the system switcher data
        '';
      };
    };

    config.closurePaths = [
      config.configuration.system.build.toplevel
      switchScript
      nixus.pkgs.execline
    ];

    config.deployScriptPhases = {
      copy-closure = lib.dag.entryBefore ["trigger-switch"] ''
        foreground {
          fdswap 1 2
          echo "Copying closure to host..."
        }

        foreground {
          define TRIES 3
          if -n {
            export NIX_SSH_OPTS "-o ServerAliveInterval=15"
            importas -i host HOST
            forbacktickx -x 0 tries { seq 1 $TRIES } # TOOD: Prevent garbage collection until the end of the deploy
            foreground { fdswap 1 2 importas -i try tries echo -en "Attempt ''${try}..." }
            nix-copy-closure ${lib.optionalString (!config.hasFastConnection) "-s"} --to $host
              "${lib.concatStringsSep "\" \"" config.closurePaths}"
          }
          echo "Failed to copy closure after ''${TRIES}"
        }
      '';

      trigger-switch = lib.dag.entryAnywhere ''
        foreground {
          fdswap 1 2
          echo "Triggering system switcher..."
        }
        backtick -i -n ID {
          importas -i host HOST
          ssh -o VisualHostKey=no -o BatchMode=yes $host
            exec "${switchScript}/bin/switch" start "${config.configuration.system.build.toplevel}"
        }
        background {
          importas -i id ID
          foreground {
            fdswap 1 2
            echo "Observing system activation ''${id}..."
          }
          importas -i host HOST
          loopwhilex -x 0
          ssh -o VisualHostKey=no -o BatchMode=yes $host
            exec ${builtins.concatStringsSep " " config.privilegeEscalationCommand}
              cat "${config.systemSwitcherDir}/system-''${id}/fifo"
        }

        foreground {
          fdswap 1 2
          echo "Trying to confirm success..."
        }
        if {
          importas -i id ID
          importas -i host HOST
          loopwhilex -x 0,2
          backtick -i -n STATUS {
            foreground {
              # TODO: Because of the imperative network-setup script, when e.g. the
              # defaultGateway is removed, the previous entry is still persisted on
              # a rebuild switch, even though with a reboot it wouldn't. Maybe use
              # the more modern and declarative networkd to get around this
              timeout --foreground 15
              ssh -o VisualHostKey=no -o ControlPath=none -o BatchMode=yes $host
                exec "${switchScript}/bin/switch" active $id
            }
            sleep 1
          }

          fdswap 1 2
          importas -i status STATUS
          ifelse { test $status = "success" } {
            foreground {
              echo "Successfully activated new system!"
            } exit 0
          }
          ifelse { test $status = "failure" } {
            foreground {
              echo "Failed to activate new system! Rolled back to previous one"
            } exit 2
          }
          exit 1
        }
      '';
    };

    config.nodeDeployScript = import ../scripts node;
  };
in {
  options = {
    defaults = lib.mkOption {
      type = lib.types.submodule nodeOptions;
    };

    deployScript = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
    };
  };

  # TODO: What about requiring either all nodes to succeed or all get rolled back?
  config.deployScript = nixus.pkgs.writeScript "deploy" ''
    #!${nixus.pkgs.execline}/bin/execlineb -Ws0
    forx -p script {
      ${lib.concatMapStringsSep "\n  "
        (node: lib.optionalString node.enabled node.nodeDeployScript)
        (lib.attrValues config.nodes)}
    } importas -i script script
    execlineb $script
  ''; # TODO: Handle signals to kill the async command
}
