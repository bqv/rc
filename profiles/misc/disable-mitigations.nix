{ ... }: {
  # Allow spectre locally for performance gains.
  security.mitigations = {
    disable = true;
    acceptRisk = true;
  };
}
