{ config, pkgs, lib, ... }:

pkgs.writeShellScriptBin "ipfscat" ''
  export IPFS_PATH='/var/lib/ipfs'
  bold="$(${pkgs.ncurses}/bin/tput bold)"
  sgr0="$(${pkgs.ncurses}/bin/tput sgr0)"
  if [ -z "$DISPLAY" ]; then
  ${pkgs.ipfs}/bin/ipfs add $@ |\
  ${pkgs.gnugrep}/bin/grep added |\
  ${pkgs.coreutils}/bin/cut -d' ' -f 2 |\
  ${pkgs.findutils}/bin/xargs -I{} echo "https://gateway.ipfs.io/ipfs/{}" |\
  ${pkgs.findutils}/bin/xargs echo $bold"Copy:"$sgr0
  else
  ${pkgs.ipfs}/bin/ipfs add $@ |\
  ${pkgs.gnugrep}/bin/grep added |\
  ${pkgs.coreutils}/bin/cut -d' ' -f 2 |\
  ${pkgs.findutils}/bin/xargs -I{} echo "https://gateway.ipfs.io/ipfs/{}" |\
  ${pkgs.xclip}/bin/xclip -i -r -f -selection primary |\
  ${pkgs.xclip}/bin/xclip -i -r -f -selection secondary |\
  ${pkgs.xclip}/bin/xclip -i -r -f -selection clipboard |\
  ${pkgs.findutils}/bin/xargs echo $bold"Copied:"$sgr0
  fi
''
