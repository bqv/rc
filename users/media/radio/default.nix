{ config, lib, pkgs, ... }:

let
  streams = import ./streams.nix;
in {
  home.file = lib.mapAttrs' (station: text: {
    name = "var/radio/${station}.m3u";
    value = {
      inherit text;
    };
  }) streams;

  emacs-loader.emms = {
    after = [ "ivy" ];
    config = ''
      (defconst bqv/radio-stations
        (let ((map (make-hash-table)))
          ${lib.concatStrings (lib.mapAttrsToList (station: url: ''
            (setf (map-elt map '${station})
                  "${url}")
          '') streams)}
          map)
        "Maps a radio station name symbol to a url string")
    '' + ''
      (defun emms-play-radio (station)
        "Play STATION using emms-play-url"
        (interactive (list (intern (ivy-read "Station" (bqv/verbose-radio-stations)
                                             :require-match t
                                             :sort t))))
        (let* ((name (car (split-string (if (symbolp station) (symbol-name station) station) "\t" t " ")))
               (url (map-elt bqv/radio-stations (intern name))))
          (assert (not (null url)))
          (emms-play-url url)))

      (defun bqv/current-radio-track (station)
        "Get current track of STATION"
        (interactive (list (intern (completing-read "Station" (map-keys bqv/radio-stations) nil t))))
        (let* ((url (map-elt bqv/radio-stations station)))
          (if (string-match-p "stream/?$" url)
              (with-current-buffer (url-retrieve-synchronously (replace-in-string "stream" "currentsong?sid=0" url))
                (let ((curr (progn
                              (point-max)
                              (let ((end (point)))
                                (forward-line 0)
                                (buffer-substring-no-properties (point) end)))))
                  (kill-buffer)
                  curr))
            nil)))

      (defun bqv/verbose-radio-stations ()
        "Play STATION using emms-play-url"
        (let ((max-len (apply #'max (map-keys-apply (lambda (sym) (length (symbol-name sym))) bqv/radio-stations))))
          (map-keys-apply (lambda (station) (format "%s%s\t[%s]" station
                                                    (make-string (- max-len (length (symbol-name station))) ? )
                                                    (or (bqv/current-radio-track station) "n/a")))
          bqv/radio-stations)))
    '';
  };
}

## Local Variables: ***
## mode: nix-dsquoted-emacslisp ***
## End: ***
