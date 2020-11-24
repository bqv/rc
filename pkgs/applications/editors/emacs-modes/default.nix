{ lib, flake, emacsPackages, fetchurl, lzip, pkgs, libffi, libtool, ... }:

let
  inherit (emacsPackages) trivialBuild emacs;
  inputs = {
    inherit (flake.inputs) emacs-bitwarden ivy-exwm flycheck-purescript;
    inherit (flake.inputs) eterm-256color envrc emacsbridge emacs-webkit;
    inherit (flake.inputs) font-lock-ext sln-mode emacs-ffi explain-pause-mode;
  };
in lib.recurseIntoAttrs rec {

  bitwarden = trivialBuild rec {
    pname = "bitwarden";
    version = src.shortRev;
    src = inputs.emacs-bitwarden;
  };

  ivy-exwm = trivialBuild rec {
    pname = "ivy-exwm";
    src = inputs.ivy-exwm;
    buildInputs = with emacsPackages; [
      exwm ivy ivy-rich
    ];
  };

  flycheck-purescript = trivialBuild rec {
    pname = "flycheck-purescript";
    src = inputs.flycheck-purescript;
    buildInputs = with emacsPackages; [
      flycheck
    ];
  };

  eterm-256color = trivialBuild rec {
    pname = "eterm-256color";
    src = inputs.eterm-256color;
    buildInputs = with emacsPackages; [
      xterm-color f
    ];
  };

  envrc = trivialBuild rec {
    pname = "envrc";
    version = src.shortRev;
    src = inputs.envrc;
    buildInputs = with emacsPackages; [
      seq
    ];
  };

  emacsbridge = trivialBuild rec {
    pname = "emacsbridge";
    version = src.shortRev;
    src = inputs.emacsbridge;
    preBuild = "cp lisp/* .";
    postInstall = "cp -r qml $out";
    buildInputs = with emacsPackages; [
      alert
    ];
  };

  emacs-webkit = trivialBuild rec {
    pname = "emacs-webkit";
    version = src.shortRev;
    src = inputs.emacs-webkit;
    buildPhase = ''
      make all
      export LD_LIBRARY_PATH=$PWD:$LD_LIBRARY_PATH
    '';
    nativeBuildInputs = with pkgs; [ pkg-config wrapGAppsHook ];
    gstBuildInputs = with pkgs; with gst_all_1; [
      gstreamer gst-libav
      gst-plugins-base
      gst-plugins-good
      gst-plugins-bad
      gst-plugins-ugly
    ];
    buildInputs = with pkgs; [
      webkitgtk
      glib gdk-pixbuf cairo
      mime-types pango gtk3
      glib-networking gsettings-desktop-schemas
      xclip notify-osd enchant
    ] ++ gstBuildInputs;

    GIO_EXTRA_MODULES = "${pkgs.glib-networking}/lib/gio/modules:${pkgs.dconf.lib}/lib/gio/modules";
    GST_PLUGIN_SYSTEM_PATH_1_0 = lib.concatMapStringsSep ":" (p: "${p}/lib/gstreamer-1.0") gstBuildInputs;
    postInstall = ''
      cp *.so *.js *.css $out/share/emacs/site-lisp/
      mkdir $out/lib && cp *.so $out/lib/
    '';
  };

  font-lock-ext = trivialBuild rec {
    pname = "font-lock-ext";
    version = src.shortRev;
    src = inputs.font-lock-ext;
  };

  sln-mode = trivialBuild rec {
    pname = "sln-mode";
    version = src.shortRev;
    src = inputs.sln-mode;
    buildInputs = with emacsPackages; [
      font-lock-ext
    ];
  };

  emacs-ffi = trivialBuild rec {
    pname = "emacs-ffi";
    version = src.shortRev;
    src = inputs.emacs-ffi;
    postPatch = ''
      sed 's%^EMACS_BUILDDIR.*$%EMACS_BUILDDIR = ${emacsPackages.emacs}%' -i Makefile
      sed 's%"ffi-module.so"%(concat (file-name-directory (or load-file-name buffer-file-name)) &)%' -i ffi.el
    '';
    buildPhase = ''
      make all
      export LD_LIBRARY_PATH=$PWD:$LD_LIBRARY_PATH
    '';
    buildInputs = with emacsPackages; [
      libffi libtool libtool.lib
    ];
    postInstall = ''
      cp *.so $out/share/emacs/site-lisp/
      mkdir $out/lib && cp *.so $out/lib/
    '';
  };

  explain-pause-mode = trivialBuild rec {
    pname = "explain-pause-mode";
    version = src.shortRev;
    src = inputs.explain-pause-mode;
  };

}
