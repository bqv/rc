{ lib, writeText, fetchFromGitHub, emacsPackages, libffi, libtool, ... }:

let
  inherit (emacsPackages) trivialBuild emacs;
in lib.recurseIntoAttrs rec {

  bitwarden = trivialBuild rec {
    pname = baseNameOf src.meta.homepage;
    version = lib.substring 0 7 src.rev;
    src = fetchFromGitHub {
      owner = "seanfarley";
      repo = "emacs-bitwarden";
      rev = "e03919ca68c32a8053ddea2ed05ecc5e454d8a43";
      sha256 = "1bbxh01856vg8acwrjbq3hyxa81zcvfby7k1j5hdfps918xy10m2";
      # date = 2019-08-08T16:55:08-07:00;
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
    pname = baseNameOf src.meta.homepage;
    version = lib.substring 0 7 src.rev;
    src = fetchFromGitHub {
      owner = "purcell";
      repo = "envrc";
      rev = "da8e306b0a562af05c5e990aced968d7fda06296";
      sha256 = "0h6zaxwf1wx831qfp3mahw7ir7l236a6f04dz54qzvqsx9593pfb";
      # date = 2020-09-15T10:03:07+12:00;
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
      rev = "617eb8fa4e75da40749a313544670052ed1f12f5";
      sha256 = "11y029fg4qar0ajpqphbqlsx0pnd547d0rlrif4x4q8147s0c9h2";
      # date = 2020-06-15T13:46:19+03:00;
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
      # date = 2017-10-30T15:45:42+01:00;
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
      # date = 2015-02-12T08:52:08+01:00;
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

  explain-pause-mode = trivialBuild rec {
    pname = baseNameOf src.meta.homepage;
    version = lib.substring 0 7 src.rev;
    src = fetchFromGitHub {
      owner = "lastquestion";
      repo = "explain-pause-mode";
      rev = "2356c8c3639cbeeb9751744dbe737267849b4b51";
      sha256 = "0frnfwqal9mrnrz6q4v7vcai26ahaw81894arff1yjw372pfgv7v";
      # date = 2020-07-27T02:27:40-07:00;
    };
    buildInputs = with emacsPackages; [
    ];
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
