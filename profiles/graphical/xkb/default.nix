{ config, lib, pkgs, ... }:

{
  services.xserver = {
    extraLayouts = {
      zz = import ./xkb/zz.nix { inherit pkgs; };
    };

    layout = "zz,gb";
    xkbModel = "pc105";
    #xkbOptions = lib.concatStringsSep "," [
    xkbOptions = [
      "terminate:ctrl_alt_bksp" # kill xserver
      "keypad:pointerkeys" # mousekeys!
      "keypad:hex" # add hex keys to level3 numpad
     #"ctrl:nocaps" # capslock is just another ctrl
      "caps:ctrl_modifier" # capslock is just another ctrl
      "lv3:ralt_switch_multikey" # altgr is level3, shift+altgr is compose
      "lv5:rwin_switch_lock"
      "kpdl:semi" # semicolon on numpad delete level3
      "numpad:shift3" # shift chooses numpad level3
      "grp:sclk_toggle" # scroll lock switches layout
    ];
  };
}
