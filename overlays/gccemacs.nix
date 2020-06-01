final: prev: rec {
  gccEmacs = (prev.emacs.override { srcRepo = true; }).overrideAttrs (o: rec {
    name = "gccemacs-${prev.lib.substring 0 8 src.rev}";

    # Fetching off the feature/native-comp git branch
    src = final.inputs.gccemacs;

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

  gccEmacsPackages = prev.lib.dontRecurseIntoAttrs (final.emacsPackagesFor gccEmacs);

  # Overridden for exwm
  emacsWithPackages = gccEmacsPackages.emacsWithPackages;
}
