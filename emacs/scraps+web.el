(when (executable-find "xdg-open")
  (with-eval-after-load 'browse-url
    (setq browse-url-generic-program "xdg-open"
          browse-url-browser-function 'browse-url-generic)))
