final: prev:
with prev; let
  libhandy = prev.libhandy.overrideAttrs (old: rec {
    version = "1.0.0";
    src = fetchgit {
      url = "https://gitlab.gnome.org/GNOME/libhandy"; rev = version;
      sha256 = "G8iLpiixv4cLdSO1uZC5vQBUQyrYZj8d6gIM4H0CfqQ=";
    };
    patches = [];
    doCheck = false;
  });
in { redditgtk =
  stdenv.mkDerivation rec {
    pname = "redditgtk";
    version = lib.substring 0 7 src.rev;
    src = fetchgit {
      url = "https://gitlab.gnome.org/GabMus/redditgtk";
      rev = "3afe3a7fc1d3ed81adb28c7c94ab24401aba0255";
      sha256 = "1yqxj006iwwpfva612ni602zzhqmj8z9y8ivgq3z56cr31xc2h49";
      # date = 2020-09-25T17:13:01+02:00;
    };
    nativeBuildInputs = [ meson cmake pkg-config ninja python3Packages.wrapPython wrapGAppsHook ];
    buildInputs = [ glib gobject-introspection gtk3 webkitgtk gtksourceview4 libhandy ];
    pythonPath = with python3Packages; [ pygobject3 praw flask dateutil ];
    preFixup = "wrapPythonPrograms";
    strictDeps = false;
  }
; }
