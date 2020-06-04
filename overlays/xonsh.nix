final: prev: with final.xontribs; rec {
  xonsh = prev.xonsh.overridePythonAttrs (o: rec {
    doCheck = false;
    propagatedBuildInputs = o.propagatedBuildInputs ++ [
      final.python3Packages.nixpkgs
      final.python3Packages.pip
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
