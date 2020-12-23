{ config, lib, pkgs, ... }:

let
  channels = {
    home-manager = {
      name = "guix-home-manager";
      url = "https://framagit.org/tyreunom/guix-home-manager.git";
      introduction = "b5f32950a8fa9c05efb27a0014f9b336bb318d69";
      fingerprint = "1EFB 0909 1F17 D28C CBF9  B13A 53D4 57B2 D636 EE82";
    };
  };
in {
  config = rec {
    xdg = {
      configFile."guix/channels.scm" = {
        text = ''(cons*
        ${lib.concatMapStringsSep "" (channel: ''
          (channel
           (name '${channel.name})
           (url "${channel.url}")
           (introduction
             (make-channel-introduction
               "${channel.introduction}"
               (openpgp-fingerprint
                 "${channel.fingerprint}"))))
       '') (builtins.attrValues channels)}
       %default-channels)'';
      };
    };
  };
}
