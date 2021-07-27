args@{ config, pkgs, lib, ... }:

# Done manually until i feel like something similarly hacky to ../emacs
{
  policies = lib.mapAttrs (_: nix: import nix args) {
   #"bin.brave" = ./brave;
    "bin.chromium" = ./chromium;
   #"bin.electricsheep" = ./electricsheep;
   #"bin.firefox" = ./firefox;
   #"bin.nginx" = ./nginx;
   #"bin.pulseaudio" = ./pulseaudio;
   #"bin.skype" = ./skypeforlinux;
   #"bin.upwork" = ./upwork;
   #"bin.waterfox" = ./waterfox;
   #"bin.wine" = ./wine;
   #"home..nw" = ./node-webkit;
  };
  includes = lib.mapAttrs (_: nix: import nix args) {
    "abstractions/freedesktop" = ./abstractions/freedesktop;
    "abstractions/site/base" = ./abstractions/site/base;
    "abstractions/site/de" = ./abstractions/site/de;
    "abstractions/pulse" = ./abstractions/pulse;
    "abstractions/pulse-deny" = ./abstractions/pulse-deny;
    "abstractions/user-download" = ./abstractions/user-download;
    "abstractions/user-tmp" = ./abstractions/user-tmp;
    "abstractions/xdg-desktop" = ./abstractions/xdg-desktop;
    "abstractions/tunables/home.d/site.git" = ./tunables/home.d/site.git;
    "abstractions/tunables/home.d/site.local" = ./tunables/home.d/site.local;
    "abstractions/tunables/xdg-user-dirs.d/site.local" = ./tunables/xdg-user-dirs.d/site.local;
    "abstractions/tunables/apparmorfs" = ./tunables/apparmorfs;
    "abstractions/tunables/dovecot" = ./tunables/dovecot;
    "abstractions/tunables/global" = ./tunables/global;
    "abstractions/tunables/home" = ./tunables/home;
    "abstractions/tunables/kernelvars" = ./tunables/kernelvars;
    "abstractions/tunables/ntpd" = ./tunables/ntpd;
    "abstractions/tunables/proc" = ./tunables/proc;
    "abstractions/tunables/securityfs" = ./tunables/securityfs;
    "abstractions/tunables/sys" = ./tunables/sys;
    "abstractions/tunables/xdg-user-dirs" = ./tunables/xdg-user-dirs;
  };
}
