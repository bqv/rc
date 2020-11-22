inputs@{ prompt-toolkit, ... }: final: prev: let
  super = {
    inherit (final.master.xonsh) overridePythonAttrs propagatedBuildInputs;
  };
  pythonOverrides = self: super: {
    prompt_toolkit = super.prompt_toolkit.overridePythonAttrs (_: {
      src = prompt-toolkit;
    });
  };
  python = (prev.lib.last super.propagatedBuildInputs).pkgs.python.override {
    self = python;
    packageOverrides = pythonOverrides;
  };
in with final.xontribs; rec {
  xonsh = super.overridePythonAttrs (o: rec {
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
