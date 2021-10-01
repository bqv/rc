(define-module (rc packages yggdrasil)
  #:use-module (rc packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages linux)
  #:use-module ((gnu packages networking) #:prefix gnu:)
  #:replace (yggdrasil))

(define-public go-github-com-arceliar-ironwood
  (package
    (name "go-github-com-arceliar-ironwood")
    (version "0.0.0-20210912013146-c2bc55bb349c")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Arceliar/ironwood")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1dfkqnkfxwlwcsk8g9r1pv84lfzgn8r1vam13zlmk81cgan2r6fx"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/Arceliar/ironwood"
        #:tests? #f
        #:phases
        (modify-phases %standard-phases
          ;; Source-only package
          (delete 'build))))
    (propagated-inputs
      `(("go-golang-org-x-crypto"
         ,go-golang-org-x-crypto)
        ("go-github-com-arceliar-phony"
         ,go-github-com-arceliar-phony)))
    (home-page
      "https://github.com/Arceliar/ironwood")
    (synopsis "Ironwood")
    (description
      "Ironwood is a routing library with a @code{net.PacketConn}-compatible interface using @code{ed25519.PublicKey}s as addresses.  Basically, you use it when you want to communicate with some other nodes in a network, but you can't guarantee that you can directly connect to every node in that network.  It was written to test improvements to / replace the routing logic in @url{https://github.com/yggdrasil-network/yggdrasil-go,Yggdrasil}, but it may be useful for other network applications.")
    (license license:mpl2.0)))

(define-public go-github-com-vividcortex-ewma
  (package
    (name "go-github-com-vividcortex-ewma")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/VividCortex/ewma")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0whx516l9nm4n41spagb605ry7kfnz1qha96mcshnfjlahhnnylq"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/VividCortex/ewma"))
    (home-page "https://github.com/VividCortex/ewma")
    (synopsis "EWMA")
    (description
      "Package ewma implements exponentially weighted moving averages.
")
    (license license:expat)))

(define-public go-github-com-cheggaaa-pb-v3
  (package
    (name "go-github-com-cheggaaa-pb-v3")
    (version "3.0.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cheggaaa/pb")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0d701s2niy39r650d1phjw19h4l27b1yfc2ih6s31f56b3zzqspx"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/cheggaaa/pb"))
    (propagated-inputs
      `(("go-golang-org-x-sys" ,go-golang-org-x-sys)
        ("go-github-com-rivo-uniseg"
         ,go-github-com-rivo-uniseg)
        ("go-github-com-mattn-go-runewidth"
         ,go-github-com-mattn-go-runewidth)
        ("go-github-com-mattn-go-isatty"
         ,go-github-com-mattn-go-isatty)
        ("go-github-com-mattn-go-colorable"
         ,go-github-com-mattn-go-colorable)
        ("go-github-com-fatih-color"
         ,go-github-com-fatih-color)
        ("go-github-com-vividcortex-ewma"
         ,go-github-com-vividcortex-ewma)))
    (home-page "https://github.com/cheggaaa/pb")
    (synopsis "Terminal progress bar for Go")
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-rivo-uniseg
  (package
    (name "go-github-com-rivo-uniseg")
    (version "0.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/rivo/uniseg")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0j7h22vfmjj562vr8gpsyrkrwp1pq9ayh5fylv24skxb467g9f0q"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/rivo/uniseg"))
    (home-page "https://github.com/rivo/uniseg")
    (synopsis "Unicode Text Segmentation for Go")
    (description
      "Package uniseg implements Unicode Text Segmentation according to Unicode
Standard Annex #29 (@url{http://unicode.org/reports/tr29/,http://unicode.org/reports/tr29/}).
")
    (license license:expat)))

(define-public go-github-com-mattn-go-runewidth
  (package
    (name "go-github-com-mattn-go-runewidth")
    (version "0.0.13")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mattn/go-runewidth")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1yir0f3wc5z5hnkwhvx5qb6nmpfb05zp2gvfjvna63s8kmla1rrn"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/mattn/go-runewidth"))
    (propagated-inputs
      `(("go-github-com-rivo-uniseg"
         ,go-github-com-rivo-uniseg)))
    (home-page
      "https://github.com/mattn/go-runewidth")
    (synopsis "go-runewidth")
    (description
      "This package provides functions to get fixed width of the character or string.")
    (license license:expat)))

(define-public go-github-com-vishvananda-netlink
  (package
    (name "go-github-com-vishvananda-netlink")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/vishvananda/netlink")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1vhl30p1gx636a088ls4h6a0l8jjyfvz79fr5w0qzdrg4qg9h08h"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/vishvananda/netlink"
        #:tests? #f))
    (propagated-inputs
      `(("go-golang-org-x-sys" ,go-golang-org-x-sys)
        ("go-github-com-vishvananda-netns"
         ,go-github-com-vishvananda-netns)))
    (home-page
      "https://github.com/vishvananda/netlink")
    (synopsis "netlink - netlink library for go")
    (description
      "Package netlink provides a simple library for netlink.  Netlink is
the interface a user-space program in linux uses to communicate with
the kernel.  It can be used to add and remove interfaces, set up ip
addresses and routes, and confiugre ipsec.  Netlink communication
requires elevated privileges, so in most cases this code needs to
be run as root.  The low level primitives for netlink are contained
in the nl subpackage.  This package attempts to provide a high-level
interface that is loosly modeled on the iproute2 cli.
")
    (license license:asl2.0)))

(define-public go-github-com-vishvananda-netns
  (package
    (name "go-github-com-vishvananda-netns")
    (version "0.0.0-20210104183010-2eb08e3e575f")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/vishvananda/netns")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0d5f1dvcps51rkfn3s2xzcqjz45ljvj2w1r8y4zz5zwf8y461ksw"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/vishvananda/netns"
        #:tests? #f))
    (propagated-inputs
      `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (home-page
      "https://github.com/vishvananda/netns")
    (synopsis "netns - network namespaces in go")
    (description
      "Package netns allows ultra-simple network namespace handling.  NsHandles
can be retrieved and set.  Note that the current namespace is thread
local so actions that set and reset namespaces should use LockOSThread
to make sure the namespace doesn't change due to a goroutine switch.
It is best to close NsHandles when you are done with them.  This can be
accomplished via a `defer ns.Close()` on the handle.  Changing namespaces
requires elevated privileges, so in most cases this code needs to be run
as root.
")
    (license license:asl2.0)))

(define-public yggdrasil
  (package
    (inherit gnu:yggdrasil)
    (name (package-name gnu:yggdrasil))
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/yggdrasil-network/yggdrasil-go")
         (commit (string-append "v" version))
         (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mmqw6w5a6ph49xd1yzb7i70xg466k9pi5sdvplhb66x68wipixh"))
       (patches (search-patches "yggdrasil-extra-config.new.patch"))))
    (propagated-inputs
      `(;("go-golang-zx2c4-com-wireguard-windows"
        ; ,go-golang-zx2c4-com-wireguard-windows)
        ("go-golang-zx2c4-com-wireguard"
         ,go-golang-zx2c4-com-wireguard)
        ("go-golang-org-x-text" ,go-golang-org-x-text)
        ("go-golang-org-x-sys" ,go-golang-org-x-sys)
        ("go-golang-org-x-net" ,go-golang-org-x-net)
        ("go-golang-org-x-crypto"
         ,go-golang-org-x-crypto)
        ("go-github-com-vishvananda-netns"
         ,go-github-com-vishvananda-netns)
        ("go-github-com-vishvananda-netlink"
         ,go-github-com-vishvananda-netlink)
        ("go-github-com-mitchellh-mapstructure"
         ,go-github-com-mitchellh-mapstructure)
        ("go-github-com-mattn-go-runewidth"
         ,go-github-com-mattn-go-runewidth)
        ("go-github-com-mattn-go-isatty"
         ,go-github-com-mattn-go-isatty)
        ("go-github-com-kardianos-minwinsvc"
         ,go-github-com-kardianos-minwinsvc)
        ("go-github-com-hjson-hjson-go"
         ,go-github-com-hjson-hjson-go)
        ("go-github-com-hashicorp-go-syslog"
         ,go-github-com-hashicorp-go-syslog)
        ("go-github-com-gologme-log"
         ,go-github-com-gologme-log)
        ("go-github-com-fatih-color"
         ,go-github-com-fatih-color)
        ("go-github-com-cheggaaa-pb-v3"
         ,go-github-com-cheggaaa-pb-v3)
        ("go-github-com-vividcortex-ewma"
         ,go-github-com-vividcortex-ewma)
        ("go-github-com-arceliar-phony"
         ,go-github-com-arceliar-phony)
        ("go-github-com-arceliar-ironwood"
         ,go-github-com-arceliar-ironwood)))
    ))

yggdrasil
