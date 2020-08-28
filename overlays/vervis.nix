final: prev: let
  inherit (final) fetchdarcs fetchgit;
  inherit (final.haskell.lib) dontHaddock unmarkBroken dontCheck doJailbreak;
  inherit (final.haskell.lib) addBuildDepend overrideCabal addBuildTool;
  haskellPackages = final.haskell.packages.ghc865.override { overrides = _: super: {
    base-noprelude = super.callHackage "base-noprelude" "4.12.0.0" {};

    git = unmarkBroken super.git; # not broken

    network = dontCheck (haskellPackages.callHackage "network" "2.6.3.6" {});
    smtp-mail = haskellPackages.callHackage "smtp-mail" "0.1.4.6" {};
    mime-mail = haskellPackages.callHackage "mime-mail" "0.4.14" {};

    persistent = haskellPackages.callHackage "persistent" "2.9.2" {};
    persistent-template = haskellPackages.callHackage "persistent-template" "2.5.4" {};
    persistent-postgresql = haskellPackages.callHackage "persistent-postgresql" "2.9.1" {};
    persistent-sqlite = haskellPackages.callHackage "persistent-sqlite" "2.9.3" { inherit (final) sqlite; };
    esqueleto = dontHaddock (dontCheck (haskellPackages.callHackage "esqueleto" "2.6.0" {}));
    persistent-mysql = haskellPackages.callHackage "persistent-mysql" "2.9.0" {};
  }; };
  cabal2nix = name: fetcher: attrs: haskellPackages.callCabal2nix name (fetcher attrs);
  deps = rec {
    darcs-lights = dontHaddock (cabal2nix "darcs-lights" fetchdarcs {
      url = "https://dev.angeley.es/s/fr33domlover/r/darcs-lights";
     #rev = "95b0fe34c220ed487885f6fbd99b40f06b9a451e";
      sha256 = "zvbB8AJYh1KEJiZuKxdNlEv5BmjMutZOElRm9QbKAqA=";
    } {});
    darcs-rev = cabal2nix "darcs-rev" fetchdarcs {
      url = "https://dev.angeley.es/s/fr33domlover/r/darcs-rev";
     #rev = "2fdbbc08134864415fc7b7ac3d38a20ec13bc283";
      sha256 = "vK/SfCOZS/EP2HOYh+6IvFpeYxMJS//7s/0NWLcLu6E=";
    } { inherit darcs-lights; };
    dvara = cabal2nix "dvara" fetchdarcs {
      url = "https://dev.angeley.es/s/fr33domlover/r/dvara";
     #rev = "e5f530ae995f2afc6f767d7c60452603f98a9545";
      sha256 = "mN0QOjSk8RgYr/YoaYVR1l9nFnpw8bKya/yD6uhc6vk=";
    } { inherit persistent-migration; };
    hit-graph = cabal2nix "hit-graph" fetchdarcs {
      url = "https://dev.angeley.es/s/fr33domlover/r/hit-graph";
     #rev = "0000000000000000000000000000000000000000";
      sha256 = "rmeDj6/kEIKLdcTbXDkcNtxz7kuUrelmKUu21mV3cX4=";
    } { inherit hit-harder; };
    hit-harder = cabal2nix "hit-harder" fetchdarcs {
      url = "https://dev.angeley.es/s/fr33domlover/r/hit-harder";
     #rev = "10275fce276792310e72c241471c60c7f4d9ffcc";
      sha256 = "ROs3hotBk1jSzDedZvU0nV77hUVua9veR01UVzPL6Cs=";
    } {};
    hit-network = dontHaddock (cabal2nix "hit-network" fetchdarcs {
      url = "https://dev.angeley.es/s/fr33domlover/r/hit-network";
     #rev = "9a8d64173e962a72427cb82e71a045c9e45a4027";
      sha256 = "zcKDDYNyPnY7e91LUC3yf/hMZOpDbOPJkY5C4u2leus=";
    } { inherit hit-graph hit-harder; });
    http-client-signature = cabal2nix "http-client-signature" fetchdarcs {
      url = "https://dev.angeley.es/s/fr33domlover/r/http-client-signature";
     #rev = "65c7c3d01d7f668479d7358263b825556a0e4da1";
      sha256 = "27VsnCz8RdiCQC8u+OEfSxfmg5du8OxwXp6uUK14l3w=";
    } { inherit http-signature; };
    http-signature = dontHaddock (cabal2nix "http-signature" fetchdarcs {
      url = "https://dev.angeley.es/s/fr33domlover/r/http-signature";
     #rev = "5617d1edf2daf5e084309f5191614e73664d3193";
      sha256 = "jnWIxwle3DgGfDMcorCd/Hy73EUcSgmU/jz8T2spOFM=";
    } {});
    persistent-email-address = cabal2nix "persistent-email-address" fetchdarcs {
      url = "https://dev.angeley.es/s/fr33domlover/r/persistent-email-address";
     #rev = "ae8c34f820be98607379ae16b1f17b11bf06e8c2";
      sha256 = "wDOi2IQk8c/724xcG8vRW53iL0Y+2j7HYSMpBTPEQa8=";
    } {};
    persistent-graph = cabal2nix "persistent-graph" fetchdarcs {
      url = "https://dev.angeley.es/s/fr33domlover/r/persistent-graph";
     #rev = "6e37c7b24d631d9883de5ab331d9334e50ff816e";
      sha256 = "DGQtxjoYkDNesWD5yrhoBnFcRB8poZtHXReYaZwLDwg=";
    } {};
    persistent-migration = cabal2nix "persistent-migration" fetchdarcs {
      url = "https://dev.angeley.es/s/fr33domlover/r/persistent-migration";
     #rev = "0000000000000000000000000000000000000000";
      sha256 = "a2ezrpQT0gxRQ/v81sKNKvg6EsYKF+Uc/w09oMSodM8=";
    } {};
    time-interval-aeson = cabal2nix "time-interval-aeson" fetchdarcs {
      url = "https://dev.angeley.es/s/fr33domlover/r/time-interval-aeson";
     #rev = "15919637951f7276e27863f879d646bd46734965";
      sha256 = "gXjWIC/5Y3vGb7pNEVj/dcNpOxRYi+umlPYMfHfjdXg=";
    } {};
    ssh = doJailbreak (cabal2nix "ssh" fetchdarcs {
      url = "https://dev.angeley.es/s/fr33domlover/r/ssh";
     #rev = "0000000000000000000000000000000000000000";
      sha256 = "+V3uW4REku8J5ZyjWKsPPnzf75atvi0KhPOU6DWEYt4=";
    } {});
    yesod-auth-account = dontCheck (cabal2nix "yesod-auth-account" fetchgit {
      url = "https://dev.angeley.es/s/fr33domlover/r/yesod-auth-account";
     #rev = "0000000000000000000000000000000000000000";
      sha256 = "7nA/j2cIBUrNlNZ4Zdm1ZxMJ729PkuygJqOmVbcL+Ac=";
    } { inherit persistent-email-address; });
    yesod-http-signature = cabal2nix "yesod-http-signature" fetchdarcs {
      url = "https://dev.angeley.es/s/fr33domlover/r/yesod-http-signature";
     #rev = "60cbae6c92aaa0bd544bf05be7ba9391156e4b60";
      sha256 = "kvPOZHTrsY9+fE8cZ+JUVti1P9pFjgxK8+QTRh8jrVk=";
    } { inherit http-client-signature http-signature; };
    yesod-mail-send = cabal2nix "yesod-mail-send" fetchdarcs {
      url = "https://dev.angeley.es/s/fr33domlover/r/yesod-mail-send";
     #rev = "95d0657cd8f25036e57659cd157ddafcc74dc2d3";
      sha256 = "OlAphEd/x6kzYPAMHNvwXvDDs/5G4GJudc/+FWxyZQc=";
    } {};
  };
in rec {
  vervis = overrideCabal (doJailbreak (dontHaddock (dontCheck (cabal2nix "vervis" fetchdarcs {
    url = "https://dev.angeley.es/s/fr33domlover/r/vervis";
   #rev = "eb7a1c26e489dd8ab8f6abc2a68b53278ccc2243";
    sha256 = "hcovmtTHkxsQBN+PdoBHDWxpT1xfDjDewhKFZZxKU88=";
  } deps)))) (drv: let
    dataDir = final.runCommand "svgfonts-fonts" {
      inherit (haskellPackages.SVGFonts) src;
    } "unpackPhase; cp -vir $sourceRoot/fonts $out";
  in {
    preBuild = ''
      sed -i 's|\$localInstallRoot|"'$out'"|g' src/Vervis/Settings.hs
      sed -i '254,255s/\<s\>/(T.unpack s)/' src/Vervis/Settings.hs
      sed -i '498s/(unpack styleName)/styleName/' src/Vervis/Handler/Repo.hs
      sed -i 's@data/LinLibertineCut.svg@${dataDir}/LinLibertineCut.svg@' src/Vervis/Application.hs

      darcs init
    '';
    passthru = {
      pkgs = haskellPackages // deps;
    };
  });
}
