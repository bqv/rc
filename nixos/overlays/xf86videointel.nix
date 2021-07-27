inputs@{...}: final: prev: {
  xorg = prev.xorg // prev.xorg.overrideScope' (self: super: rec {
    xf86videointel = super.xf86videointel.overrideAttrs (drv: {
      buildInputs = drv.buildInputs ++ [ self.libXv ];
    });
  });
}
