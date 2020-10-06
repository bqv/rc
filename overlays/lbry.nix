final: prev: {
  lbry = prev.callPackage (
    { stdenv, lib, fetchurl, system, appimageTools, makeWrapper, autoPatchelfHook, desktop-file-utils, imagemagick,
      ffmpeg, nss, nssTools, nspr, gtk3, vivaldi-ffmpeg-codecs, xorg }:

    stdenv.mkDerivation rec {
      pname = "lbry";
      version = "0.48.0";

      src = let
        appimage = fetchurl {
          url = "https://github.com/lbryio/lbry-desktop/releases/download/v${version}/LBRY_${version}.AppImage";
          sha256 = "LeSnIC4M2QjhyIi5IVNzw6y21kJoRWIhZw2G8O1cVHQ=";
          name = "${pname}-${version}.AppImage";
        };
      in appimageTools.extract {
        name = "${pname}-${version}";
        src = appimage;
      };

      nativeBuildInputs = [ autoPatchelfHook desktop-file-utils imagemagick makeWrapper ];
      buildInputs = [ ffmpeg nss nssTools nspr gtk3 vivaldi-ffmpeg-codecs ] ++
        (with xorg; [ libXcomposite libXrender libXtst ]);

      installPhase = ''
        runHook preInstall

        mkdir -p $out
        cp -r ${src}/{*.pak,*.bin,*.dat,resources,locales} $out

        for size in 16 32 48 64 72 96 128 192 256 512 1024; do
        mkdir -p $out/share/icons/hicolor/"$size"x"$size"/apps
        convert -resize "$size"x"$size" \
          ${src}/usr/share/icons/hicolor/256x256/apps/lbry.png \
          $out/share/icons/hicolor/"$size"x"$size"/apps/lbry.png
        done

        desktop-file-install --dir $out/share/applications \
          --set-key Exec --set-value lbry \
          --set-key Icon --set-value lbry \
          ${src}/lbry.desktop

        install -Dm755 ${src}/lbry $out/lbry
        makeWrapper $out/lbry $out/bin/lbry \
          --run "cd $out" \
          --argv0 lbry

        runHook postInstall
      '';
    }
  ) {};
}
