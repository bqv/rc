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
    src = fetchGit { url = "https://gitlab.gnome.org/GabMus/redditgtk"; rev = "c7406bc5fc5927d5b7c461698af2e69635c13542"; };
    nativeBuildInputs = [ meson cmake pkg-config ninja python3Packages.wrapPython wrapGAppsHook ];
    buildInputs = [ glib gobject-introspection gtk3 webkitgtk gtksourceview4 libhandy ];
    pythonPath = with python3Packages; [ pygobject3 praw flask dateutil ];
    preFixup = "wrapPythonPrograms";
    strictDeps = false;
  }
; }
