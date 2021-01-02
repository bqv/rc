inputs@{ giara, pr99188, ... }: final: prev: {
  inherit (inputs.pr99188.legacyPackages.${final.system}) giara-init;
  giara = final.callPackage (
    { lib
    , fetchFromGitLab
    , meson
    , gobject-introspection
    , pkg-config
    , ninja
    , python3
    , wrapGAppsHook
    , gtk3
    , gdk-pixbuf
    , webkitgtk
    , gtksourceview4
    , libhandy
    , glib-networking
    }:
    
    python3.pkgs.buildPythonApplication rec {
      pname = "giara";
      version = "0.2";
    
      format = "other";
    
      src = inputs.giara;
    
      nativeBuildInputs = [
        meson
        gobject-introspection
        pkg-config
        ninja
        wrapGAppsHook
      ];
    
      buildInputs = [
        gtk3
        gdk-pixbuf
        webkitgtk
        gtksourceview4
        libhandy
        glib-networking
      ];
    
      pythonPath = with python3.pkgs; [
        pygobject3
        pycairo
        dateutil
        praw
        pillow
        mistune
        beautifulsoup4
      ];
    
      # Fix setup-hooks https://github.com/NixOS/nixpkgs/issues/56943
      strictDeps = false;
    
      meta = with lib; {
        description = "A Reddit app, built with Python, GTK and Handy. Created with mobile Linux in mind.";
        maintainers = with maintainers; [ atemu ];
        homepage = "https://gitlab.gnome.org/World/giara";
        license = licenses.gpl3Plus;
        platforms = platforms.linux;
      };
    }
  ) {};
}
