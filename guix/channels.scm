;(list (channel
;       (name 'guix)
;       (url "https://git.savannah.gnu.org/git/guix.git")
;       (commit "6700dc3341bedb70e98bfa9f5eeb8db68a7c31b3")))
(cons* (channel
        (name 'guix-home-manager)
        (url "https://framagit.org/tyreunom/guix-home-manager.git")
        (introduction
          (make-channel-introduction
            "b5f32950a8fa9c05efb27a0014f9b336bb318d69"
            (openpgp-fingerprint
              "1EFB 0909 1F17 D28C CBF9  B13A 53D4 57B2 D636 EE82"))))
       %default-channels)
