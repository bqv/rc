inputs@{ ... }: final: prev: let
  super = {
    inherit (prev.xonsh) overridePythonAttrs propagatedBuildInputs;
  };
  pythonOverrides = self: super: {
    prompt_toolkit = super.prompt_toolkit.overridePythonAttrs (_: rec {
      version = "3.0.11";
      src = self.fetchPypi {
        pname = "prompt_toolkit";
        inherit version;
        sha256 = "3IPmNosO3ZzqvhegVfLiL27ZW5qjnb1Z0LTzWFvf6e0=";
      };
    });
  };
  python = let
    py = prev.lib.findFirst (p: p.pname == "python3") null super.propagatedBuildInputs;
  in if prev.lib.versionAtLeast py.pkgs.prompt_toolkit.version "3.0.11"
  then py else py.pkgs.python.override {
    self = python;
    packageOverrides = pythonOverrides;
  };
in with final.xontribs; rec {
  xonsh = super.overridePythonAttrs (o: rec {
    doCheck = false;
    propagatedBuildInputs = [
      python.pkgs.ply
      python.pkgs.prompt_toolkit

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
