{ config, lib, usr, pkgs, ... }:

{
  emacs-loader.fix-input = {
    demand = true;
    config = ''
      ;; ensure wayland xkb
      (when (eq window-system 'pgtk)
        (pgtk-use-im-context t))

      ;; update british ime
      (with-temp-buffer
        (activate-input-method "british")
        (let ((quail-current-package (assoc "british" quail-package-alist)))
          (quail-define-rules ((append . t))
                              ("\"" ["@"])
                              ("@" ["\""]))))

      ;; reverse compose british with ims
      (fix-input "british" "programmer-dvorak" "uk-programmer-dvorak")
      (fix-input "british" "english-dvorak" "uk-dvorak")
      (fix-input "british" "TeX" "uk-TeX")

      ;; merge imes for great success (ideally...)
      (with-temp-buffer
        (activate-input-method "uk-programmer-dvorak")
        (activate-input-method "TeX")
        (let ((pkg (copy-tree (quail-package "uk-programmer-dvorak"))))
          (setcar pkg "uk-tex-programmer-dvorak")
          (setcar (cdr pkg) "\\DVP@")
          (setcar (nth 2 pkg) (append (cdr (nth 2 (quail-package "uk-programmer-dvorak")))
                                      (cdr (nth 2 (quail-package "TeX")))))
          (quail-add-package pkg))
        (register-input-method "uk-tex-programmer-dvorak" "UTF-8" 'quail-use-package "\\DVP@"))

      ;; res ipsa loquitur
      (setq default-input-method "uk-programmer-dvorak")
    '';
  };
}
