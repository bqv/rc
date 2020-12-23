inputs@{ ini2json, ... }: final: prev: {
  ini2json = final.stdenv.mkDerivation {
    name = "ini2json";
    src = inputs.ini2json;
    nativeBuildInputs = [ final.cmake ];
  };

  # Uses IFD because why the heck not
  callINI = iniFile: builtins.fromJSON (builtins.readFile (
    final.runCommand "${iniFile}.json" {
      ini = builtins.readFile iniFile;
      buildInputs = [ ini2json ];
      passAsFile = [ "ini" ];
    } ''
      ln -s $iniPath $out.ini
      ini2json $out.ini > $out
    ''
  ));
}
