{ appimageTools, fetchurl, lib, gsettings-desktop-schemas, gtk3, makeDesktopItem }:

let
  pname = "electronmail";
  version = "4.4.2";
  desktopItem = makeDesktopItem {
     name = "ElectronMail";
     exec = "electron-mail";
     type = "Application";
     desktopName = "ElectronMail";
  };
in appimageTools.wrapType2 rec {
  name = "${pname}-${version}";
  src = fetchurl {
    url = "https://github.com/vladimiry/${pname}/releases/download/v${version}/electron-mail-${version}-linux-x86_64.AppImage";
    sha256 = "1zl0sybzc86wqxgpiqa3hsdmb3nw9jskbps3j336v024zmzv2ih1";
  };

 #profile = ''
 #  export LC_ALL=C.UTF-8
 #  export XDG_DATA_DIRS=${gsettings-desktop-schemas}/share/gsettings-schemas/${gsettings-desktop-schemas.name}:${gtk3}/share/gsettings-schemas/${gtk3.name}:$XDG_DATA_DIRS
 #'';

  multiPkgs = null; # no 32bit needed
  extraPkgs = appimageTools.defaultFhsEnvArgs.multiPkgs;
  extraInstallCommands = ''
    mkdir -p $out/share/applications
    ln -s ${desktopItem}/share/applications/* $out/share/applications
    mv $out/bin/{${name},${pname}}
  '';

  meta = with lib; {
    description = "Unofficial ProtonMail Desktop App";
    homepage = "https://github.com/vladimiry/ElectronMail/";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    platforms = [ "x86_64-linux" ];
  };
}
