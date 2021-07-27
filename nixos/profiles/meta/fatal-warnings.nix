{ config, ... }: {
  assertions = builtins.map (w: {
    assertion = false;
    message = w;
  }) config.warnings;
}
