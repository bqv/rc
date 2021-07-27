inputs@{ rel2003, ... }: final: prev: {
  postman = (final.symlinkJoin {
    name = "postman";
    paths = [
      (final.writeScriptBin "postman-x11" ''
        #!${final.execline}/bin/execlineb -S0
        export DISPLAY :0
        exec -a postman
        ${inputs.rel2003.postman}/bin/postman
      '')
      inputs.rel2003.postman
    ];
  }).overrideAttrs (_: { inherit (inputs.rel2003.postman) meta; });
}
