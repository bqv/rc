final: prev: {
  epsxe = final.insecuressl.epsxe.overrideAttrs ({ installPhase, ... }: {
    installPhase = let
      bios = final.fetchurl {
        url = "https://ps1emulator.com/SCPH1001.BIN";
        hash = "sha256-ca+U0eR6aMEej9ufg2gEBgFRSkKlo5nNpIx9O/8emdM=";
      };
    in installPhase + ''
      mkdir -p $out/share/bios/
      ln -s ${bios} $out/share/bios/scph1001.bin
    '';
  });
}
