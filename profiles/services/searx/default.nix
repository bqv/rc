{ config, pkgs, lib, ... }:

let
  cfg = config.services.searx;
in lib.mkIf cfg.enable {
  services.searx.settings = {
    general.debug = false; # breaks at runtime otherwise, somehow
    search.safe_search = 0;
    search.autocomplete = "qwant";
    search.default_lang = "en-GB";
    server.bind_address = "0.0.0.0";
    server.secret_key = "87dd9e896bdb4b7cac32fd7f90867f87";
    server.image_proxy = false;
    server.default_locale = "en";
    ui.default_theme = "oscar";
    ui.theme_args.oscar_style = "logicodev-dark";
    engines = {
      "bitbucket".disabled = false;
      "ccc-tv".disabled = false;
      "ddg definitions".disabled = false;
      "erowid".disabled = false;
      "duckduckgo".disabled = false;
      "duckduckgo images".disabled = false;
      "fdroid".disabled = false;
      "gitlab".disabled = false;
      "google play apps".disabled = false;
      "nyaa".disabled = false;
      "openrepos".disabled = false;
      "qwant".disabled = false;
      "reddit".disabled = false;
      "searchcode code".disabled = false;
      "framalibre".disabled = false;
      "wikibooks".disabled = false;
      "wikinews".disabled = false;
      "wikiquote".disabled = false;
      "wikisource".disabled = false;
      "wiktionary".disabled = false;
    };
  };
}
