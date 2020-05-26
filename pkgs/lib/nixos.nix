{ pkgs }:

let
  shFlagsRules = rules: ''
    . ${pkgs.shflags}
    ${rules}
    FLAGS "$@" || exit $?
    eval set -- "''${FLAGS_ARGV}"
  '';
  rebuild = operation: ''
    REV=$(git rev-parse HEAD)
    FLAKE=$(git rev-parse --show-toplevel)

    ARGS="--use-remote-sudo"

    if [ -z "$FLAGS_host" ]; then
      ARGS="$ARGS --flake $FLAKE"
    else
      ARGS="$ARGS --flake $FLAKE#$FLAGS_host"
    fi

    if [ $FLAGS_showtrace -eq $FLAGS_TRUE ]; then
      ARGS="$ARGS --show-trace"
    fi

    if [ $FLAGS_verbose -eq $FLAGS_TRUE ]; then
      ARGS="$ARGS -vv"
    fi

    if [ $FLAGS_verynoisy -eq $FLAGS_TRUE ]; then
      ARGS="$ARGS -vvvvv"
    fi

    if [ $FLAGS_remote -eq $FLAGS_TRUE ]; then
      ARGS="$ARGS -j0"
    fi

    echo '> nixos-rebuild' $ARGS ${operation}
    source $(which nixos-rebuild) $ARGS ${operation}
  '';
in pkgs.writeShellScriptBin "nixos" ''
  ${shFlagsRules ''
    DEFINE_string 'host' "" 'Host to build' 'H'
    DEFINE_boolean 'showtrace' false 'Show verbose traces' 'T'
    DEFINE_boolean 'verbose' false 'Show verbose logs' 'v'
    DEFINE_boolean 'verynoisy' false 'Show noisy logs' 'V'
    DEFINE_boolean 'remote' false 'Force disable local building' 'R'

    DEFINE_boolean 'check' false 'Run flake checks only' 'c'
    DEFINE_boolean 'build' false 'Attempt to build the system' 'b'
    DEFINE_boolean 'activate' false 'Activate the system now' 'a'
    DEFINE_boolean 'setdefault' false 'Set default boot configuration' 's'
    DEFINE_boolean 'tagactive' false 'Force tag current configuration only' 't'
  ''}

  DATE=$(date -d @$(git log --pretty=format:"%cd" --date=unix $REV -1) +%Y%m%D.%H%M%S)
  export NIXOS_LABEL="$DATE-''${REV:0:7}"

  if [ $FLAGS_check -eq $FLAGS_TRUE ]; then
    if [ $FLAGS_showtrace -eq $FLAGS_TRUE ]; then
      ARGS="$ARGS --show-trace"
    fi

    if [ $FLAGS_verbose -eq $FLAGS_TRUE ]; then
      ARGS="$ARGS -vv"
    fi

    if [ $FLAGS_verynoisy -eq $FLAGS_TRUE ]; then
      ARGS="$ARGS -vvvvv"
    fi

    echo '> nix flake check' $ARGS
    exec nix flake check $ARGS
  fi

  if [ $FLAGS_tagactive -eq $FLAGS_FALSE ]; then
    if [ $FLAGS_build -eq $FLAGS_FALSE ]; then
      ${rebuild "dry-build"}
    else
      if [ $FLAGS_activate -eq $FLAGS_FALSE ]; then
        if [ $FLAGS_setdefault -eq $FLAGS_FALSE ]; then
          ${rebuild "dry-activate"}
        else
          ${rebuild "boot"}
        fi
      else
        if [ $FLAGS_setdefault -eq $FLAGS_FALSE ]; then
          ${rebuild "test"}
        else
          ${rebuild "switch"}
        fi
      fi
    fi
  else
    REV=$(git rev-parse HEAD)
    pathToConfig=$(readlink -f /run/current-system)
  fi

  if [ $FLAGS_activate -eq $FLAGS_TRUE ] ||
     [ $FLAGS_tagactive -eq $FLAGS_TRUE ]; then
    if [ -e "$pathToConfig" ]; then
      SYSTEMPATH=$(basename $pathToConfig)
      PROFILES=($(find /nix/var/nix/profiles/ -lname $pathToConfig))
      echo Tagging $SYSTEMPATH && git tag $SYSTEMPATH $REV || true
      for profile in $PROFILES; do
        SYSTEMNUM=$(hostname)/$(basename $profile)
        echo Tagging $SYSTEMNUM && git tag $SYSTEMNUM $REV || true
      done
    fi
  elif [ $FLAGS_build -eq $FLAGS_TRUE ]; then
    if [ -e "$pathToConfig" ]; then
      SYSTEMPATH=$(basename $pathToConfig)
      echo Tagging $SYSTEMPATH && git tag $SYSTEMPATH $REV || true
    fi
  fi
''
