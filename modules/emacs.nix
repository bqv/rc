{ super, config, lib, pkgs, usr, ... }:

let
  packageNames = lib.mapAttrsToList (_: e: e.name) config.emacs.loader;
  packageSpec = { config, name, ... }:
  {
    options = with lib; {
      enable = mkEnableOption "this package" // {
        default = true;
      };
      name = mkOption {
        type = types.str;
        default = name;
      };
      demand = mkOption {
        type = types.bool;
        default = false;
      };
      defer = mkOption {
        type = types.bool;
        default = false;
      };
      require = mkOption {
        type = types.listOf types.str;
        default = [];
      };
      after = mkOption {
        type = types.listOf (types.enum packageNames);
        default = [];
      };
      diminish = mkOption {
        type = types.listOf types.str;
        default = [];
      };
      commands = mkOption {
        type = types.listOf types.str;
        default = [];
      };
      hook = mkOption {
        type = types.listOf (types.addCheck
          (types.attrsOf types.str)
          (e: builtins.length (builtins.attrNames e) == 1));
        default = [];
      };
      bind = mkOption {
        type = types.attrsOf types.str;
        default = {};
      };
      mode = mkOption {
        type = types.attrsOf types.str;
        default = {};
      };
      package = mkOption {
        type = types.anything;
        default = epkgs: epkgs.${config.name};
        defaultText = "epkgs: epkgs.${config.name}";
      };
      init = mkOption {
        type = types.lines;
        default = "";
      };
      config = mkOption {
        type = types.lines;
        default = "";
      };
      initPkg = mkOption {
        type = types.anything;
        default = epkgs: epkgs.trivialBuild rec {
          pname = "${name}-init";
          src = usr.elisp.writeFile {
            name = pname;
            description = "";
            text = config.init;
          };
        };
      };
      configPkg = mkOption {
        type = types.anything;
        default = epkgs: epkgs.trivialBuild rec {
          pname = "${name}-config";
          src = usr.elisp.writeFile {
            name = pname;
            description = "";
            text = config.config;
          };
        };
      };
      script = mkOption {
        type = types.anything;
        default = epkgs: let
          initPkg = config.initPkg epkgs;
          configPkg = config.configPkg epkgs;

          initSrc = "${initPkg}/share/emacs/site-lisp/${initPkg.pname}";
          configSrc = "${configPkg}/share/emacs/site-lisp/${configPkg.pname}";
          notWhitespace = s: builtins.isNull (builtins.match "[ \\n]*" s);

          attrToCons = attr: let car = builtins.head (builtins.attrNames attr);
                                 cdr = builtins.head (builtins.attrValues attr);
                             in "(${car} . ${cdr})";
          attrsToCons = set: lib.mapAttrsToList (k: v: attrToCons { "${k}" = v; }) set;
          attrsToQCons = lib.mapAttrsToList (k: v: attrToCons { "\"${k}\"" = v; });

          attrs = {
            name = (assert ! lib.hasInfix " " config.name; config.name);
            demand = if config.demand then ":leaf-defer nil" else "";
            defer = if config.defer then ":defer t" else "";
            require = if builtins.length config.require <= 0 then ""
                      else if builtins.length config.require == 1
                      then ":require ${builtins.head config.require}"
                      else ":require (${lib.concatStringsSep " " config.require})";
            after = if builtins.length config.after <= 0 then ""
                    else if builtins.length config.after == 1
                    then ":after ${builtins.head config.after}"
                    else ":after (${lib.concatStringsSep " " config.after})";
            diminish =#if builtins.length config.diminish <= 0 then ""
                  "" ;#else if builtins.length config.diminish == 1
                      #then ":diminish ${builtins.head config.diminish}"
                      #else ":diminish (${lib.concatStringsSep " " config.diminish})";
            commands = if builtins.length config.commands <= 0 then ""
                       else if builtins.length config.commands == 1
                       then ":commands ${builtins.head config.commands}"
                       else ":commands (${lib.concatStringsSep " " config.commands})";
            hook = if builtins.length config.hook <= 0 then ""
                   else if builtins.length config.hook == 1
                   then ":hook ${attrToCons (builtins.head config.hook)}"
                   else ":hook (${lib.concatMapStringsSep " " attrToCons config.hook})";
            bind = if builtins.length (builtins.attrNames config.bind) <= 0 then ""
                   else if builtins.length (builtins.attrNames config.bind) == 1
                   then ":bind ${builtins.head (attrsToQCons config.bind)}"
                   else ":bind (${lib.concatStringsSep " " (attrsToQCons config.bind)})";
            mode = if builtins.length (builtins.attrNames config.mode) <= 0 then ""
                   else if builtins.length (builtins.attrNames config.mode) == 1
                   then ":mode ${builtins.head (attrsToCons config.mode)}"
                   else ":mode (${lib.concatStringsSep " " (attrsToCons config.mode)})";
            init = if notWhitespace config.init
                   then ":init (load-file \"${initSrc}.el\")"
                   else "";
            config = if notWhitespace config.config
                     then ":config (load-file \"${configSrc}.el\")"
                     else "";
          };
        in ''
          (leaf ${attrs.name}
            ${attrs.demand}
            ${attrs.defer}
            ${attrs.require}
            ${attrs.after}
            ${attrs.diminish}
            ${attrs.commands}
            ${attrs.hook}
            ${attrs.bind}
            ${attrs.mode}
            ; (load-file "~/site-lisp/${initPkg.pname}.el")
            ${attrs.init}
            ; (load-file "~/site-lisp/${configPkg.pname}.el")
            ${attrs.config})
        '';
      };
      systemDeps = mkOption {
        type = types.listOf types.package;
        default = [];
      };
    };
  };
in {
  options = with lib; {
    emacs = {
      loader = lib.mkOption {
        type = types.attrsOf (types.submodule packageSpec);
        default = {};
      };
      package = lib.mkOption {
        type = types.package;
      };
    };
  };
}
