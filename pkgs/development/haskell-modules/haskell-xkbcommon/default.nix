{ mkDerivation, base, bytestring, Cabal, cpphs, data-flags
, directory, filepath, libxkbcommon, process, random, stdenv
, storable-record, template-haskell, text, time, transformers, unix
, vector, fetchFromGitHub, writeText }:

let cpphs-patched = cpphs.overrideAttrs (o: {
  patches = let
    templateHaskellPatch = writeText "cpphs.patch" ''
      diff -rN -u base-cpphs/Language/Preprocessor/Cpphs/Tokenise.hs ongy-cpphs/Language/Preprocessor/Cpphs/Tokenise.hs
      --- base-cpphs/Language/Preprocessor/Cpphs/Tokenise.hs	2018-05-08 16:57:53.934565562 +0200
      +++ ongy-cpphs/Language/Preprocessor/Cpphs/Tokenise.hs	2018-05-08 16:57:53.934565562 +0200
      @@ -204,6 +204,8 @@
           lexcpp LineComment w l ls (_:xs)      = lexcpp LineComment (' ':w) l ls xs
           lexcpp (NestComment _) w l ls ('*':'/':xs)
                                                 = lexcpp Any [] (w*/*l) ls xs
      +    lexcpp (NestComment _) w l ls xs@('\n':_)
      +                                          = lexcpp Any [] (w*/*l) ls xs
           lexcpp (NestComment n) w l ls (x:xs)  = lexcpp (NestComment n) (white x:w) l
                                                                               ls xs
           lexcpp mode w l ((p,l'):ls) []        = cpp mode next w l p ls ('\n':l')
    '';
  in (o.patches or []) ++ [
    templateHaskellPatch
  ];
});
in let cpphs = cpphs-patched;
in mkDerivation {
  pname = "xkbcommon";
  version = "0.0.3";
  src = fetchFromGitHub {
    owner = "ongy";
    repo = "haskell-xkbcommon";
    rev = "a1455b51c053289b1b5b27d19d4159180647228d";
    sha256 = "03sw3isp64p1pqdwsagkvbppl8yqg7gwji5h7c80kci8dsx351zr";
  };
  doCheck = false;
  setupHaskellDepends = [
    base Cabal cpphs directory filepath process template-haskell text
  ];
  libraryHaskellDepends = [
    base bytestring cpphs data-flags filepath process storable-record
    template-haskell text transformers
  ];
  librarySystemDepends = [ libxkbcommon ];
  testHaskellDepends = [ base unix ];
  benchmarkHaskellDepends = [ base random time vector ];
  description = "Haskell bindings for libxkbcommon";
  license = stdenv.lib.licenses.mit;
}
