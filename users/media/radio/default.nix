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
    config = ''
      (defconst bqv/radio-stations
        (let ((map (make-hash-table)))
          ${lib.concatStrings (lib.mapAttrsToList (station: url: ''
            (setf (map-elt map '${station})
                  "${url}")
          '') streams)}
          map)
        "Maps a radio station name symbol to a url string")

      (defun emms-play-radio (station)
        "Play STATION using emms-play-url"
        (interactive (list (intern (completing-read "Station" (map-keys bqv/radio-stations) nil t))))
        (let* ((url (map-elt bqv/radio-stations station)))
          (assert (not (null url)))
          (emms-play-url url)))
    '';
  };
}
