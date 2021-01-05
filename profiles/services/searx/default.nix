{ config, pkgs, lib, ... }:

let
  cfg = config.services.searx;
in lib.mkIf cfg.enable {
  services.searx.configFile = let
    defaultJSON = pkgs.runCommand "searx.defaults.json" {} ''
      ${pkgs.remarshal}/bin/remarshal -if yaml -of json ${pkgs.searx.src}/searx/settings.yml -o $out
    '';
    defaults = builtins.fromJSON (builtins.readFile defaultJSON);
    engines = lib.recursiveUpdate (builtins.listToAttrs (builtins.map (value: {
      inherit (value) name;
      inherit value;
    }) defaults.engines)) {
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
    settings = lib.recursiveUpdate defaults {
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
      engines = builtins.attrValues engines;
    };
    settingJSON = builtins.toFile "searx.settings.json" (builtins.toJSON settings);
  in pkgs.runCommand "searx.settings.yml" rec {
    json = settingJSON;
    passthru = settings;
  } ''
    ${pkgs.remarshal}/bin/remarshal -if json -of yaml $json -o $out
  '';
}
