{ ... }: {
  # Disable mitigations for performance gains.
  security.mitigations = {
    disable = true;
    acceptRisk = true;
  };
}
