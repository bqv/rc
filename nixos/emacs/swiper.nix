{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.swiper = {
    demand = true;
    after = [ "ivy" ];
    bind = {
      "M-s" = "swiper";
    };
  };
}
