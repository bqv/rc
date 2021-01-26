{ ... }: {
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };
  security.doas = {
    enable = true;
    wheelNeedsPassword = false;
  };
}
