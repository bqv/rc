{ lib, writeText, fetchFromGitHub, emacsPackages, libffi, libtool, ... }:

let
  inherit (emacsPackages) trivialBuild emacs;
in lib.recurseIntoAttrs rec {

  bitwarden = trivialBuild rec {
    pname = "emacs-bitwarden";
    src = fetchFromGitHub {
      owner = "seanfarley"; repo = pname;
      rev = "e03919ca68c32a8053ddea2ed05ecc5e454d8a43";
      sha256 = "1bbxh01856vg8acwrjbq3hyxa81zcvfby7k1j5hdfps918xy10m2";
    };
  };

  ivy-exwm = trivialBuild rec {
    pname = "ivy-exwm";
    src = fetchFromGitHub {
      owner = "pjones"; repo = pname;
      rev = "32f107374aef01b9ae00f1647233d50b4ea659e0";
      sha256 = "1shs1zh8nr2lwxlvrhnhxxjn5g0p21vkjxnjgha1sn07pg7v3iqq";
    };
    buildInputs = with emacsPackages; [
      exwm ivy ivy-rich
    ];
  };

  flycheck-purescript = trivialBuild rec {
    pname = "flycheck-purescript";
    src = fetchFromGitHub {
      owner = "bsermons"; repo = pname;
      rev = "a3f5e64fe56aedf9703540b4755a2e6e044cbe72";
      sha256 = "0qm048ypfzbrqd4a9ffn1ay3rhh58nacd9z78lph8mmj4ri1v2cc";
    };
    buildInputs = with emacsPackages; [
      flycheck
    ];
  };

  eterm-256color = trivialBuild rec {
    pname = "eterm-256color";
    src = fetchFromGitHub {
      owner = "dieggsy"; repo = pname;
      rev = "0f0dab497239ebedbc9c4a48b3ec8cce4a47e980";
      sha256 = "00ins8n92p5aspr6bjrvn5y5w0ximakk22yklsfmkav4h10al4as";
    };
    buildInputs = with emacsPackages; [
      xterm-color f
    ];
  };

  envrc = trivialBuild rec {
    pname = "envrc";
    version = "20200611.254";
    src = fetchFromGitHub {
      owner = "purcell"; repo = pname;
      rev = "aae15dd8c4736ceb515b09b1db9eef3db3bd65d9";
      sha256 = "14f5hh09qdjdy757h3lg36z5x2nrnx2s0dmzm1jlk4q7qj274114";
    };
    buildInputs = with emacsPackages; [
      seq
    ];
  };

  emacsbridge = trivialBuild rec {
    pname = baseNameOf src.meta.homepage;
    version = lib.substring 0 7 src.rev;
    src = fetchFromGitHub {
      owner = "aardsoft";
      repo = "emacsbridge";
      rev = "c11e18940f5e662ddd79519ab043d30114f4a7c6";
      sha256 = "1lkipb4x57pscy62qc3r3zacpmyd85frq7rld2v3psfayxrsb9l6";
    };
    preBuild = "cp lisp/* .";
    postInstall = "cp -r qml $out";
    buildInputs = with emacsPackages; [
      alert
    ];
  };

  font-lock-ext = trivialBuild rec {
    pname = baseNameOf src.meta.homepage;
    version = lib.substring 0 7 src.rev;
    src = fetchFromGitHub {
      owner = "sensorflo";
      repo = "font-lock-ext";
      rev = "b6c82e8ac7996d96494a54454015a98ceb883feb";
      sha256 = "19wywl3y5schs9yzvrnngamajvgbbvajxhgpzfprwwv660phdsvl";
    };
    buildInputs = with emacsPackages; [
    ];
  };

  sln-mode = trivialBuild rec {
    pname = baseNameOf src.meta.homepage;
    version = lib.substring 0 7 src.rev;
    src = fetchFromGitHub {
      owner = "sensorflo";
      repo = "sln-mode";
      rev = "0f91d1b957c7d2a7bab9278ec57b54d57f1dbd9c";
      sha256 = "1ahrripvlrislw3b40zkvxnsbwkzagxdbcskn92kpf8944zjmaay";
    };
    buildInputs = with emacsPackages; [
      font-lock-ext
    ];
  };

  emacs-ffi = trivialBuild rec {
    pname = baseNameOf src.meta.homepage;
    version = lib.substring 0 7 src.rev;
    src = fetchFromGitHub {
      owner = "tromey";
      repo = "emacs-ffi";
      rev = "cc19a6c2098f54b7254b1c94727f0eae26d8c5b1";
      sha256 = "0lqybbk3k7p4jnazc69d9a8yndbd6cddagmynkyk975vznbnzpqp";
      # date = 2017-12-06T09:35:52-07:00;
    };
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

  _0xc = null;
  _2048-game = null;
  _4clojure = null;
  at = null;
  emacs-libvterm = null;
  emacsClangCompleteAsync = null;
  term-plus = null;
  term-plus-key-intercept = null;
  term-plus-mux = null;
  xml-plus = null;
}
