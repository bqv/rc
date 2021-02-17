{ config, lib, usr, pkgs, ... }:

{
  emacs.loader.weechat = {
    demand = true;
    package = epkgs: epkgs.weechat-patched;
    after = [ "tracking" ];
    init = ''
      (require 'bindat)
      (defun bindat--unpack-group (spec)
        (with-suppressed-warnings ((lexical struct last))
          (defvar struct) (defvar last))
        (let (struct last)
          (while spec
            (let* ((item (car spec))
                   (field (car item))
                   (type (nth 1 item))
                   (len (nth 2 item))
                   (vectype (and (eq type 'vec) (nth 3 item)))
                   (tail 3)
                   data)
              (setq spec (cdr spec))
              (if (and (consp field) (eq (car field) 'eval))
                  (setq field (eval (car (cdr field)) t)))
              (if (and type (consp type) (eq (car type) 'eval))
                  (setq type (eval (car (cdr type)) t)))
              (if (and len (consp len) (eq (car len) 'eval))
                  (setq len (eval (car (cdr len)) t)))
              (if (memq field '(eval fill align struct union))
                  (setq tail 2
                        len type
                        type field
                        field nil))
              (if (and (consp len) (not (eq type 'eval)))
                  (setq len (apply #'bindat-get-field struct len)))
              (if (not len)
                  (setq len 1))
              (cond
               ((eq type 'eval)
                (if field
                    (setq data (eval len t))
                  (eval len t)))
               ((eq type 'fill)
                (setq bindat-idx (+ bindat-idx len)))
               ((eq type 'align)
                (while (/= (% bindat-idx len) 0)
                  (setq bindat-idx (1+ bindat-idx))))
               ((eq type 'struct)
                (setq data (bindat--unpack-group (eval len t))))
               ((eq type 'repeat)
                (let ((index 0) (count len))
                  (while (< index count)
                    (push (bindat--unpack-group (nthcdr tail item)) data)
                    (setq index (1+ index)))
                  (setq data (nreverse data))))
               ((eq type 'union)
                (with-suppressed-warnings ((lexical tag))
                  (defvar tag))
                (let ((tag len) (cases (nthcdr tail item)) case cc)
                  (while cases
                    (setq case (car cases)
                          cases (cdr cases)
                          cc (car case))
                    (if (or (equal cc tag) (equal cc t)
                            (and (consp cc) (eval cc t)))
                        (setq data (bindat--unpack-group (cdr case))
                              cases nil)))))
               (t
                (setq data (bindat--unpack-item type len vectype)
                      last data)))
              (if data
                  (setq struct (if field
                                   (cons (cons field data) struct)
                                 (append data struct))))))
          struct))

      (defun bindat--length-group (struct spec)
        (with-suppressed-warnings ((lexical struct last))
          (defvar struct) (defvar last))
        (let ((struct struct) last)
          (while spec
            (let* ((item (car spec))
                   (field (car item))
                   (type (nth 1 item))
                   (len (nth 2 item))
                   (vectype (and (eq type 'vec) (nth 3 item)))
                   (tail 3))
              (setq spec (cdr spec))
              (if (and (consp field) (eq (car field) 'eval))
                  (setq field (eval (car (cdr field)) t)))
              (if (and type (consp type) (eq (car type) 'eval))
                  (setq type (eval (car (cdr type)) t)))
              (if (and len (consp len) (eq (car len) 'eval))
                  (setq len (eval (car (cdr len)) t)))
              (if (memq field '(eval fill align struct union))
                  (setq tail 2
                        len type
                        type field
                        field nil))
              (if (and (consp len) (not (eq type 'eval)))
                  (setq len (apply #'bindat-get-field struct len)))
              (if (not len)
                  (setq len 1))
              (while (eq type 'vec)
                (if (consp vectype)
                    (setq len (* len (nth 1 vectype))
                          type (nth 2 vectype))
                  (setq type (or vectype 'u8)
                        vectype nil)))
              (cond
               ((eq type 'eval)
                (if field
                    (setq struct (cons (cons field (eval len t)) struct))
                  (eval len t)))
               ((eq type 'fill)
                (setq bindat-idx (+ bindat-idx len)))
               ((eq type 'align)
                (while (/= (% bindat-idx len) 0)
                  (setq bindat-idx (1+ bindat-idx))))
               ((eq type 'struct)
                (bindat--length-group
                 (if field (bindat-get-field struct field) struct) (eval len t)))
               ((eq type 'repeat)
                (let ((index 0) (count len))
                  (while (< index count)
                    (bindat--length-group
                     (nth index (bindat-get-field struct field))
                     (nthcdr tail item))
                    (setq index (1+ index)))))
               ((eq type 'union)
                (with-suppressed-warnings ((lexical tag))
                  (defvar tag))
                (let ((tag len) (cases (nthcdr tail item)) case cc)
                  (while cases
                    (setq case (car cases)
                          cases (cdr cases)
                          cc (car case))
                    (if (or (equal cc tag) (equal cc t)
                            (and (consp cc) (eval cc t)))
                        (progn
                          (bindat--length-group struct (cdr case))
                          (setq cases nil))))))
               (t
                (if (setq type (assq type bindat--fixed-length-alist))
                    (setq len (* len (cdr type))))
                (if field
                    (setq last (bindat-get-field struct field)))
                (setq bindat-idx (+ bindat-idx len))))))))

      (defun bindat--pack-group (struct spec)
        (with-suppressed-warnings ((lexical struct last))
          (defvar struct) (defvar last))
        (let ((struct struct) last)
          (while spec
            (let* ((item (car spec))
                   (field (car item))
                   (type (nth 1 item))
                   (len (nth 2 item))
                   (vectype (and (eq type 'vec) (nth 3 item)))
                   (tail 3))
              (setq spec (cdr spec))
              (if (and (consp field) (eq (car field) 'eval))
                  (setq field (eval (car (cdr field)) t)))
              (if (and type (consp type) (eq (car type) 'eval))
                  (setq type (eval (car (cdr type)) t)))
              (if (and len (consp len) (eq (car len) 'eval))
                  (setq len (eval (car (cdr len)) t)))
              (if (memq field '(eval fill align struct union))
                  (setq tail 2
                        len type
                        type field
                        field nil))
              (if (and (consp len) (not (eq type 'eval)))
                  (setq len (apply #'bindat-get-field struct len)))
              (if (not len)
                  (setq len 1))
              (cond
               ((eq type 'eval)
                (if field
                    (setq struct (cons (cons field (eval len t)) struct))
                  (eval len t)))
               ((eq type 'fill)
                (setq bindat-idx (+ bindat-idx len)))
               ((eq type 'align)
                (while (/= (% bindat-idx len) 0)
                  (setq bindat-idx (1+ bindat-idx))))
               ((eq type 'struct)
                (bindat--pack-group
                 (if field (bindat-get-field struct field) struct) (eval len t)))
               ((eq type 'repeat)
                (let ((index 0) (count len))
                  (while (< index count)
                    (bindat--pack-group
                     (nth index (bindat-get-field struct field))
                     (nthcdr tail item))
                    (setq index (1+ index)))))
               ((eq type 'union)
                (with-suppressed-warnings ((lexical tag))
                  (defvar tag))
                (let ((tag len) (cases (nthcdr tail item)) case cc)
                  (while cases
                    (setq case (car cases)
                          cases (cdr cases)
                          cc (car case))
                    (if (or (equal cc tag) (equal cc t)
                            (and (consp cc) (eval cc t)))
                        (progn
                          (bindat--pack-group struct (cdr case))
                          (setq cases nil))))))
               (t
                (setq last (bindat-get-field struct field))
                (bindat--pack-item last type len vectype)
                ))))))
    '';
    config = let
      creds = usr.secrets.weechat.credentials;
    in ''
      (setq weechat-auto-monitor-buffers t
            weechat-completing-read-function 'weechat--try-ivy
            weechat-complete-nick-postfix ": "
            weechat-read-marker-always-show t
            weechat-max-nick-length 12
            ;weechat-modules '(weechat-button weechat-complete weechat-spelling weechat-tracking weechat-notifications weechat-image weechat-read-marker weechat-color))
            weechat-modules '(weechat-button weechat-complete weechat-spelling weechat-tracking weechat-image weechat-read-marker weechat-color))
      ;(setq weechat-auto-monitor-buffers '("weechat" "relay.list" "fset" "exec.0"))
      (setq weechat-color-list '(unspecified "aquamarine4" "PaleGreen3" "LemonChiffon4" "burlywood" "LightGoldenrod2" "tan2" "LightSalmon2" "coral2" "IndianRed3" "IndianRed" "MediumPurple3" "PaleVioletRed2" "HotPink3" "CadetBlue" "SteelBlue3" "DarkSeaGreen"))
      (dolist (module weechat-modules nil)
        (require module))
      (dolist (buffer (buffer-list) nil)
        (with-current-buffer buffer
          (when (eq major-mode 'weechat-mode)
            (tracking-add-buffer buffer))))
      (defun bqv/weechat (&rest args)
        "Connect to WeeChat [zeta]."
        (interactive)
        (weechat-connect "zeta.${usr.secrets.domains.home}" 6697 "${creds.password}" 'ssl))
      (defun bqv/weechat-local (&rest args)
        "Connect to WeeChat [localhost]."
        (interactive)
        (let ((password (auth-source-pick-first-password :user '("weechat")
                                                         :type 'netrc
                                                         :max 1)))
          (weechat-connect "localhost" 6697 password 'ssl)))
      (setcar (cddddr (alist-get 'weechat-button-url-regexp weechat-button-list)) #'browse-url-generic)
      (setq weechat-color-options-list
            (let ((amap (mapcar*
                         #'(lambda (kv c) (cons (car kv) c))
                         (seq-filter #'(lambda (kv) (stringp (cdr kv))) weechat-color-options-list)
                         `(,@(cdr weechat-color-list) ,@(cdr weechat-color-list))
                         )))
              (mapcar
               #'(lambda (c) (if-let ((m (assoc (car c) amap))) m c))
               weechat-color-options-list)))
      (defun weechat--color-handle-F (str i old-face)
      "Handle ?F (A)STD|(A)EXT color code in STR at I with OLD-FACE.
    This is an internal function of `weechat-handle-color-codes'."
      (let (match-data
            face
            (j (1+ i)))
        (while (setq match-data (weechat--match-color-code 'attr str j)) ;; (A)
          (if (eq (cl-third match-data) 'keep)
              (setq face (weechat--color-keep-attributes old-face))
            (setq face (append (list (cl-third match-data)) face)))
          (setq j (cl-second match-data)))
        (setq match-data (weechat--match-color-code 'std str j))
        (if match-data
            (setq face (append (list (list :foreground (nth (cl-third match-data)
                                                            weechat-color-list)))
                               face)) ;; TODO set attribute instead of simply append
          (setq match-data (weechat--match-color-code 'ext str j))
          (if match-data
              (setq face `((:foreground ,(nth (mod (cl-third match-data) (length weechat-color-list)) weechat-color-list)))) ;; TODO
            (weechat-relay-log (format "Broken color code (in ?F '%s' %s)" str i)
                               :warn)))
        (if match-data
            (cl-values (cl-second match-data)
                       face)
          (cl-values j face))))
      (defun weechat-handle-color-codes (str)
        "Convert the Weechat color codes in STR to properties.
    EXT colors are currently not supported.  Any color codes left are stripped.
    Be aware that Weechat does not use mIRC color codes.
    See URL `http://www.weechat.org/files/doc/devel/weechat_dev.en.html#color_codes_in_strings'."
        (let ((i 0)
              (str (s-replace "" "" str))
              (default-face
                (progn
                  (setq weechat-handle-color-codes--offset
                        (not (and (boundp 'weechat-handle-color-codes--offset) weechat-handle-color-codes--offset)))
                  (if weechat-handle-color-codes--offset nil nil)))
                    ;'gainsboro
                    ;'white)))
              face
              (ret "")
              (len (length str)))
          (while (< i len)
            (cl-case (aref str i)
              ((?\x19) ;; STD|EXT|?F((A)STD|(A)EXT)|?B(STD|EXT)|?\x1C|?*...|?b...
               (let ((old-face face)
                     (next (aref str (1+ i))))
                 (setq face default-face)
                 (setq i (1+ i))
                 (cond
                  ((and (<= ?0 next) (<= next ?9)) ;; STD
                   (let ((match-data (weechat--match-color-code 'std str i)))
                     (when match-data
                       (setq face (weechat--color-std-to-theme (cl-third match-data)))
                       (setq i (cl-second match-data)))))
                  ((= next ?@) ;; EXT
                   (let ((match-data (weechat--match-color-code 'ext str i)))
                     (when match-data
                       (setq face `((:foreground ,(nth (mod (cl-third match-data) (length weechat-color-list)) weechat-color-list)))) ;; TODO
                       (setq i (cl-second match-data)))))
                  ((= next ?F) ;; ?F(A)STD|?F(A)EXT
                   (cl-multiple-value-setq (i face) (weechat--color-handle-F str i old-face)))
                  ((= next ?B) ;; ?BSTD|?BEXT
                   (let ((match-data (weechat--match-color-code 'std str i)))
                     (if match-data
                         (setq face (list (list :background (nth (cl-third match-data)
                                                                 weechat-color-list))))
                       (setq match-data (weechat--match-color-code 'ext str i))
                       (if match-data
                           t ;; TODO ext
                         (weechat-relay-log (format "Broken color code (in ?B '%s' %s)" str i)
                                            :warn)))
                     (setq i (if match-data
                                 (cl-second match-data)
                               (1+ i)))))
                  ((= next ?*) ;; (A)STD | (A)EXT | (A)STD ?, (A)STD | ...
                   (cl-multiple-value-setq (i face) (weechat--color-handle-F str i old-face))
                   (when (= (aref str i) ?,)
                     (setq i (1+ i))
                     (let ((match-data (weechat--match-color-code 'std str i)))
                       (if match-data
                           (setq face (append (list (list :background (nth (cl-third match-data)
                                                                           weechat-color-list)))
                                              face))
                         (setq match-data (weechat--match-color-code 'ext str i))
                         (if match-data
                             t ;; TODO ext
                           (weechat-relay-log (format "Broken color code (in ?* '%s' %s)" str i)
                                              :warn)))
                       (setq i (if match-data
                                   (cl-second match-data)
                                 (1+ i))))))
                  ((= next ?b) 'b) ;; ignore for now
                  ((= next ?\x1C)  ;; Clear color, leave attributes
                   (setq face (weechat--color-keep-attributes old-face))))))

              ((?\x1A) ;; Set ATTR
               (let ((match-data (weechat--match-color-code 'attr str (1+ i))))
                 (if (not match-data)
                     (progn
                       (weechat-relay-log (format "Broken color code (in ?\\x1A '%s' %s)" str i)
                                          :warn)
                       (setq i (1+ i)))
                   (if (eq (cl-third match-data) 'keep)
                       (setq face (weechat--color-keep-attributes face))
                     (setq face (list (cl-third match-data))))
                   (setq i (cl-second match-data)))))

              ((?\x1B) ;; Delete ATTR
               (let ((match-data (weechat--match-color-code 'attr str (1+ i)))
                     (old-face (copy-sequence face)))
                 (if (not match-data)
                     (progn
                       (weechat-relay-log (format "Broken color code (in ?\\x1B '%s' %s)" str i)
                                          :warn)
                       (setq i (1+ i)))
                   (if (eq (cl-third match-data) 'keep)
                       (setq face default-face) ;; TODO Does keep here means delete all or keep all?
                     (setq face (delq (cl-third match-data) old-face)))
                   (setq i (cl-second match-data)))))

              ((?\x1C) (setq i (1+ i)
                             face default-face))) ;; reset face

            (let ((r (string-match-p "\\(\x19\\|\x1A\\|\x1B\\|\x1C\\)" str i)))
              (if r
                  (setq ret (concat ret
                                    (propertize  (substring str i r) 'face (or face 'default)))
                        i r)
                (setq ret (concat ret (propertize (substring str i) 'face (or face 'default)))
                      i len)))) ;; STOP
          ret))
      (defun weechat-buffer-name (buffer-ptr)
        (let ((hash (weechat-buffer-hash buffer-ptr)))
          (let* ((raw-name (or (gethash "name"        hash)
                               (gethash "full_name"   hash)))
                 (raw-parts (split-string raw-name "[.]"))
                 (short-name (gethash "short_name" hash))
                 (local-vars (gethash "local_variables" hash))
                 (server (assoc-default "server" local-vars))
                 (invalid-char-re "[^a-zA-Z0-9_-]"))
            (cond
             ((s-equals? (assoc-default "script_name" local-vars) "matrix")
              (format "matrix.%s" (replace-regexp-in-string "\s" "_" short-name)))
             ((s-equals? (assoc-default "plugin" local-vars) "weecord")
              (format "discord.%s.#%s"
                      (replace-regexp-in-string invalid-char-re "" server)
                      (replace-regexp-in-string invalid-char-re "" short-name)))
             (t raw-name)))))
      (defun weechat-read-marker--set-overlay ()
        "Update the `after-string' property on an already existing overlay."
        (let* ((widths (mapcar (lambda (w) (with-selected-window w (window-width)))
                               (get-buffer-window-list (current-buffer) nil t)))
               (width (- (if (null widths) 1 (apply #'min widths)) 1))
               (line (make-string width weechat-read-marker-char)))
          (overlay-put weechat-read-marker-overlay 'after-string
                       (concat "\n" (propertize line 'face 'weechat-read-marker)))))
      (evil-set-initial-state 'weechat-mode 'emacs)

      (add-hook 'after-init-hook #'bqv/weechat-local)

      (defun weechat-allout-setup (&rest _)
        (outline-minor-mode 0)
        (setq-local outline-regexp "^[^ ]* [a-zA-Z]\\|^[^ ]*  \\*\\|^\\[")
        (allout-mode 1)
        (setq-local allout-primary-bullet "^<.*>\\|^\\[.*\\] ")
        (setq-local comment-start "^<.*>\\|^\\[.*\\] ")
        t)

      (add-hook 'weechat-mode-hook #'outline-minor-mode)
      (add-hook 'weechat-mode-hook #'weechat-allout-setup))
    '';
  };
}
