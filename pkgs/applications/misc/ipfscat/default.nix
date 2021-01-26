{ config, pkgs, lib, ... }:

let
  gateway = "https://gateway.ipfs.io";
in pkgs.symlinkJoin {
  name = "ipfscat";
  paths = [
    (pkgs.writeShellScriptBin "ipfscat" ''
      export IPFS_PATH='/var/lib/ipfs'
      bold="$(${pkgs.ncurses}/bin/tput bold)"
      sgr0="$(${pkgs.ncurses}/bin/tput sgr0)"
      if [ -z "$DISPLAY" ] && [ "$DISPLAY" != "wayland-0" ]; then
        ${pkgs.ipfs}/bin/ipfs add --pin=false $@ |\
        ${pkgs.gnugrep}/bin/grep added |\
        ${pkgs.coreutils}/bin/cut -d' ' -f 2 |\
        ${pkgs.findutils}/bin/xargs -I{} echo "${gateway}/ipfs/{}" |\
        ${pkgs.findutils}/bin/xargs echo $bold"Copy:"$sgr0
      else
        ${pkgs.ipfs}/bin/ipfs add --pin=false $@ |\
        ${pkgs.gnugrep}/bin/grep added |\
        ${pkgs.coreutils}/bin/cut -d' ' -f 2 |\
        ${pkgs.findutils}/bin/xargs -I{} echo "${gateway}/ipfs/{}" |\
        ${pkgs.xclip}/bin/xclip -i -r -f -selection primary |\
        ${pkgs.xclip}/bin/xclip -i -r -f -selection secondary |\
        ${pkgs.xclip}/bin/xclip -i -r -f -selection clipboard |\
        ${pkgs.findutils}/bin/xargs echo $bold"Copied:"$sgr0
      fi
    '')
    (pkgs.writeShellScriptBin "ipfsclustercat" ''
      bold="$(${pkgs.ncurses}/bin/tput bold)"
      sgr0="$(${pkgs.ncurses}/bin/tput sgr0)"
      if [ -z "$DISPLAY" ] && [ "$DISPLAY" != "wayland-0" ]; then
        ${pkgs.ipfs-cluster}/bin/ipfs-cluster-ctl add --local -rHQ $@ |\
        ${pkgs.findutils}/bin/xargs -I{} echo "${gateway}/ipfs/{}" |\
        ${pkgs.findutils}/bin/xargs echo $bold"Copy:"$sgr0
      else
        ${pkgs.ipfs-cluster}/bin/ipfs-cluster-ctl add --local -rHQ $@ |\
        ${pkgs.findutils}/bin/xargs -I{} echo "${gateway}/ipfs/{}" |\
        ${pkgs.xclip}/bin/xclip -i -r -f -selection primary |\
        ${pkgs.xclip}/bin/xclip -i -r -f -selection secondary |\
        ${pkgs.xclip}/bin/xclip -i -r -f -selection clipboard |\
        ${pkgs.findutils}/bin/xargs echo $bold"Copied:"$sgr0
      fi
    '')
  ];
}
