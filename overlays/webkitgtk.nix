final: prev: {
  webkitgtk-eme = prev.webkitgtk.overrideAttrs (super: {
    buildInputs = super.buildInputs ++ (with final; [
      libgpgerror
    ]);
    cmakeFlags = super.cmakeFlags ++ [
      "-DENABLE_ENCRYPTED_MEDIA=ON"
    ];
  });
}
