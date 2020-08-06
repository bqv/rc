final: prev: let
  pythonOverrides = self: super: {
    prompt_toolkit = super.prompt_toolkit.overridePythonAttrs (_: {
      src = final.fetchFromGitHub {
        owner = "bobhy";
        repo = "python-prompt-toolkit";
        rev = "th-threadsafe-load-2";
        sha256 = "sha256-P6/r07bifrib0Pti83d2q5cKYr1q6jxRpT44539kctE=";
      };
    });
  };
  python = final.python3.override {
    self = python;
    packageOverrides = pythonOverrides;
  };
in with final.xontribs; rec {
  xonsh = prev.xonsh.overridePythonAttrs (o: rec {
    doCheck = false;
    propagatedBuildInputs = [
      python.pkgs.ply
      python.pkgs.prompt_toolkit
      python.pkgs.pygments

      python.pkgs.nixpkgs
      python.pkgs.pip
      apt-tabcomplete
      autoxsh
      avox
      base16-shell
      direnv
      #docker-tabcomplete
      fzf-widgets
      #hist-navigator
      histcpy
      #kitty
      output-search
      pipeliner
      powerline
      prompt-bar
      prompt-vi-mode
      pure
      pyenv
      readable-traceback
      schedule
      scrapy-tabcomplete
      #ssh-agent
      vox-tabcomplete
      xo
      z
    ];
  });
}
