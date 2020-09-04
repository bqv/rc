{ config, lib, pkgs, name, ... }:

let
  beginPhase = ''
    export HOST "${config.host}"
    foreground {
      fdswap 1 2
      echo "Connecting to host..."
    }

    backtick -i -n OLDSYSTEM {
      if -n -t {
        importas -i host HOST
        timeout --foreground 30
        ssh -o VisualHostKey=no -o ControlPath=none -o BatchMode=yes $host
          realpath /run/current-system
      }
      foreground {
        fdswap 1 2
        echo "Unable to connect to host!"
      } exit 1
    }

    ifelse {
      importas -i system OLDSYSTEM
      test $system = "${config.configuration.system.build.toplevel}"
    } {
      fdswap 1 2
      echo "No deploy necessary"
    }
    foreground {
      fdswap 1 2
      importas -i system OLDSYSTEM
      echo "Deploying ''${system}"
    }
  '';

  endPhase = ''
    foreground {
      fdswap 1 2
      echo "Finished"
    } exit 0
  '';

  errorNoHost = ''
    foreground {
      fdswap 1 2
      echo "Don't know how to reach node, you need to set a non-null value for nodes.<hostname>.host"
    } exit 1
  '';
in pkgs.writeScript "deploy-${name}" ''
  #!${pkgs.execline}/bin/execlineb -Ws0
  importas -i syspath PATH
  export PATH "${lib.makeBinPath config.deployScriptPath}:''${syspath}"

  # Prefix all output with host name
  pipeline -w { sed -u "s/^/[${name}] /" }
  fdswap 1 2
  pipeline -w { sed -u "s/^/[${name}] /" }
  fdswap 2 1

  ${if config.host == null then ''
    ${errorNoHost}
  '' else ''
    # ======== BEGINPHASE ========
    ${beginPhase}

    ${lib.concatMapStringsSep "\n" ({ name, data }: ''
      # ======== PHASE: ${name} ========
      ${data}
    '') ((lib.dag.topoSort config.deployScriptPhases).result
      or (throw "Cycle in DAG for deployScriptPhases"))}

    # ======== ENDPHASE ========
    ${endPhase}

    exit 0
  ''}
''
