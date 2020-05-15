final: prev: rec {
  gccEmacs = (prev.emacs.override { srcRepo = true; }).overrideAttrs (o: rec {
    name = "gccemacs-${prev.lib.substring 0 8 src.rev}";
    src = prev.fetchFromGitHub {
      owner = "emacs-mirror";
      repo = "emacs";
      # Fetching off the feature/native-comp git branch
      rev = "92dc81f85e1b91db04487ccf1b52c0cd3328dfee";
      sha256 = "1f22bxwq53hhdjlakmqz66y63vix5ybpnc1pk9fpy18wjh871scq";
    };
    patches = [];
    postPatch = ''
      substituteInPlace lisp/loadup.el \
        --replace '(emacs-repository-get-version)' '"${src.rev}"' \
        --replace '(emacs-repository-get-branch)' '"feature/native-comp"'
    '';

    # When this is enabled, emacs does native compilation lazily after starting
    # up, resulting in quicker package builds up-front, at the cost of slower
    # running emacs until everything has been compiled.
    #makeFlags = [ "NATIVE_FAST_BOOT=1" ];

    LIBRARY_PATH = "${prev.lib.getLib prev.stdenv.cc.libc}/lib";

    configureFlags = o.configureFlags ++ [ "--with-nativecomp" ];

    buildInputs = with final; o.buildInputs ++ [ jansson libgccjit glibc harfbuzz.dev ];
  });

  # Overridden for exwm
  emacsWithPackages = (prev.emacsPackagesFor gccEmacs).emacsWithPackages;
}
