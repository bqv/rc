final: prev: {
  vervis = let
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
     #} rec {
     #  HsOpenSSL = haskellPackages.HsOpenSSL.override { inherit network; };
     #  network = dontCheck (haskellPackages.callHackage "network" "2.6.3.6" {});
     #});
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
    vervis = overrideCabal (doJailbreak (cabal2nix "vervis" fetchdarcs {
      url = "https://dev.angeley.es/s/fr33domlover/r/vervis";
     #rev = "eb7a1c26e489dd8ab8f6abc2a68b53278ccc2243";
      sha256 = "hcovmtTHkxsQBN+PdoBHDWxpT1xfDjDewhKFZZxKU88=";
    } deps)) (drv: {
      preBuild = '' sed -i 's|\$localInstallRoot|"'$out'"|g' src/Vervis/Settings.hs '';
    });
    vervisPkgs = final.haskell-nix.project {
      src = final.runCommand "vervis" {
        vervis = vervis.src;
        darcs_lights = deps.darcs-lights.src;
        darcs_rev = deps.darcs-rev.src;
        dvara = deps.dvara.src;
        hit_graph = deps.hit-graph.src;
        hit_harder = deps.hit-harder.src;
        hit_network = deps.hit-network.src;
        http_client_signature = deps.http-client-signature.src;
        http_signature = deps.http-signature.src;
        persistent_email_address = deps.persistent-email-address.src;
        persistent_graph = deps.persistent-graph.src;
        persistent_migration = deps.persistent-migration.src;
        time_interval_aeson = deps.time-interval-aeson.src;
        ssh = deps.ssh.src;
        yesod_auth_account = deps.yesod-auth-account.src;
        yesod_http_signature = deps.yesod-http-signature.src;
        yesod_mail_send = deps.yesod-mail-send.src;
      } ''
        mkdir -p $out
        cp -r $vervis/* $out/
        mkdir -p $out/lib
        cp -r $darcs_lights $out/lib/darcs-lights
        cp -r $darcs_rev $out/lib/darcs-rev
        cp -r $dvara $out/lib/dvara
        cp -r $hit_graph $out/lib/hit-graph
        cp -r $hit_harder $out/lib/hit-harder
        cp -r $hit_network $out/lib/hit-network
        cp -r $http_client_signature $out/lib/http-client-signature
        cp -r $http_signature $out/lib/http-signature
        cp -r $persistent_email_address $out/lib/persistent-email-address
        cp -r $persistent_graph $out/lib/persistent-graph
        cp -r $persistent_migration $out/lib/persistent-migration
        cp -r $time_interval_aeson $out/lib/time-interval-aeson
        cp -r $ssh $out/lib/ssh
        cp -r $yesod_auth_account $out/lib/yesod-auth-account
        cp -r $yesod_http_signature $out/lib/yesod-http-signature
        cp -r $yesod_mail_send $out/lib/yesod-mail-send
      '';
      stack-sha256 = "nLMMb/tngIG6VU4kH7dPr9DCPTYYz9G9J9ko45l91x0=";
      sha256map = {
        "https://dev.angeley.es/s/fr33domlover/r/yesod-auth-account"."2d19eea0fae58897a02372a84cc48e7696a4e288" =
          "/D7e+zMq/1bsHN3D8mNkoU/dZZIdiM4M7DC+GKUMKag=";
      };
      pkg-def-extras = [
        (hackage: { hsc2hs = hackage.hsc2hs."0.68.4".revisions.default; })
      ];
      #compiler-nix-name = "ghc883";
    };
  in  vervis;
 #final.symlinkJoin {
 #  name = "vervis";
 #  buildInputs = [];
 #  paths = builtins.attrValues vervisPkgs.vervis.components.exes;
 #  postBuild = ''
 #    true
 #  '';
 #};
}
