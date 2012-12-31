{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.undo-tree = {
    demand = true;
    after = [ "evil" ];
    config = ''
      (global-undo-tree-mode t)
      (evil-set-undo-system 'undo-tree)
      (setq undo-tree-enable-undo-in-region nil)
      (setq undo-tree-visualizer-diff t)
      (setq undo-tree-visualizer-timestamps t)
    '';
  };
}
