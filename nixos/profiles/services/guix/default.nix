{ config, pkgs, ... }:

{
  services.guix.enable = true;
  services.guix.package = pkgs.guix;

  environment.etc."guix/acl".text = ''
    (acl 
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     (entry 
      (public-key 
       (ecc 
        (curve Ed25519)
        (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
        )
       )
      (tag 
       (guix import)
       )
      )
     )
  '';
}
