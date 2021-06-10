(define-module (rc packages mdbook)
  #:use-module (rc packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages crates-io)
  #:export (rust-mdbook-0.4))

(define rust-select-0.5
  (package
    (name "rust-select")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "select" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dvnb12fqczq0mqgyh7pafkhngv8478x0y3sxy5ngj7w1bwn3q4f"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bit-set" ,rust-bit-set-0.5)
         ("rust-html5ever" ,rust-html5ever-0.25)
         ("rust-markup5ever-rcdom"
          ,rust-markup5ever-rcdom-0.1))))
    (home-page
      "https://github.com/utkarshkukreti/select.rs")
    (synopsis
      "A library to extract useful data from HTML documents, suitable for web scraping.")
    (description
      "This package provides a library to extract useful data from HTML documents, suitable for web scraping.")
    (license license:expat)))

(define rust-shlex-1
  (package
    (name "rust-shlex")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "shlex" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0gf773p2snqpw69rzh8s1wdlq8dc8c1ypmiv516il1fdyb46i9a2"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/comex/rust-shlex")
    (synopsis
      "Split a string into shell words, like Python's shlex.
")
    (description
      "Split a string into shell words, like Python's shlex.
")
    (license (list license:expat license:asl2.0))))

(define rust-pulldown-cmark-0.7
  (package
    (name "rust-pulldown-cmark")
    (version "0.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pulldown-cmark" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0m8z8svkw3cl29gacj6vlmda9a3nfnqf9j550khrfx8q9nlxwdna"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bitflags" ,rust-bitflags-1)
         ("rust-getopts" ,rust-getopts-0.2)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-unicase" ,rust-unicase-2))))
    (home-page
      "https://github.com/raphlinus/pulldown-cmark")
    (synopsis "A pull parser for CommonMark")
    (description
      "This package provides a pull parser for CommonMark")
    (license license:expat)))

(define rust-smartstring-0.2
  (package
    (name "rust-smartstring")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "smartstring" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1sdsrppmazvzrwbdivqzkfsrp0i6aqbyp7bql7w4rvzq1da8gnhs"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-arbitrary" ,rust-arbitrary-0.4)
         ("rust-proptest" ,rust-proptest-0.10)
         ("rust-serde" ,rust-serde-1)
         ("rust-static-assertions"
          ,rust-static-assertions-1))))
    (home-page
      "https://github.com/bodil/smartstring")
    (synopsis "Compact inlined strings")
    (description "Compact inlined strings")
    (license #f)))

(define rust-socket2-0.4
  (package
    (name "rust-socket2")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "socket2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18ny6m1gnf6cwp5ax0b5hr36w6yg16z7faj76b31aq2jghhgqgcy"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-libc" ,rust-libc-0.2)
         ("rust-winapi" ,rust-winapi-0.3))))
    (home-page
      "https://github.com/rust-lang/socket2")
    (synopsis
      "Utilities for handling networking sockets with a maximal amount of configuration
possible intended.
")
    (description
      "Utilities for handling networking sockets with a maximal amount of configuration
possible intended.
")
    (license (list license:expat license:asl2.0))))

(define rust-postgres-derive-0.4
  (package
    (name "rust-postgres-derive")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "postgres-derive" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0xqlf1gffy3q8hra3fm0vm9x8i5fkvi0qjl99d0xirxh3hidsmy8"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page
      "https://github.com/sfackler/rust-postgres")
    (synopsis
      "An internal crate used by postgres-types")
    (description
      "An internal crate used by postgres-types")
    (license (list license:expat license:asl2.0))))

(define rust-geo-types-0.6
  (package
    (name "rust-geo-types")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "geo-types" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wivy6r2bzc32gxp5g5j689qz6p9ls5qgq0z8q64aayv3xd950vm"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-approx" ,rust-approx-0.3)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-rstar" ,rust-rstar-0.8)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/georust/geo")
    (synopsis "Geospatial primitive data types")
    (description "Geospatial primitive data types")
    (license (list license:expat license:asl2.0))))

(define rust-pdqselect-0.1
  (package
    (name "rust-pdqselect")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pdqselect" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09vwywzavhgqgr3vi2ycgv2nd0067jirv36fb3jvp860xikigjaf"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "")
    (synopsis
      "Selects the kth smallest element of a slice, based on Orson Peters's pdqsort")
    (description
      "Selects the kth smallest element of a slice, based on Orson Peters's pdqsort")
    (license (list license:asl2.0 license:expat))))

(define rust-rstar-0.8
  (package
    (name "rust-rstar")
    (version "0.8.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rstar" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1b6vjfwvpcgy0q8ywywz548vhxrmhbz2sm6xyhnmj5p5xd1xfqff"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-heapless" ,rust-heapless-0.6)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-pdqselect" ,rust-pdqselect-0.1)
         ("rust-serde" ,rust-serde-1)
         ("rust-smallvec" ,rust-smallvec-1))))
    (home-page "https://github.com/Stoeoef/rstar")
    (synopsis
      "R*-tree library for the rust ecosystem")
    (description
      "R*-tree library for the rust ecosystem")
    (license (list license:expat license:asl2.0))))

(define rust-approx-0.4
  (package
    (name "rust-approx")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "approx" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0y52dg58lapl4pp1kqlznfw1blbki0nx6b0aw8kja2yi3gyhaaiz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-num-complex" ,rust-num-complex-0.3)
         ("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page
      "https://github.com/brendanzab/approx")
    (synopsis
      "Approximate floating point equality comparisons and assertions.")
    (description
      "Approximate floating point equality comparisons and assertions.")
    (license license:asl2.0)))

(define rust-geo-types-0.7
  (package
    (name "rust-geo-types")
    (version "0.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "geo-types" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1fyj8ldl4xirhr8aawwiaslkklkzml2r5var87vqyp4zvnajxgfq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-approx" ,rust-approx-0.4)
         ("rust-arbitrary" ,rust-arbitrary-1)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-rstar" ,rust-rstar-0.8)
         ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/georust/geo")
    (synopsis "Geospatial primitive data types")
    (description "Geospatial primitive data types")
    (license (list license:expat license:asl2.0))))

(define rust-eui48-0.4
  (package
    (name "rust-eui48")
    (version "0.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "eui48" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0sqbmcnvilanzjagknmpf85pnji2b9hn2pqzd5rygrfkwikghk4c"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-rustc-serialize"
          ,rust-rustc-serialize-0.3)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/abaumhauer/eui48")
    (synopsis
      "A library to generate and parse IEEE EUI-48 and EUI-64, also known as MAC-48 media access
control addresses. The IEEE claims trademarks on the names EUI-48 and EUI-64, in which EUI is an
abbreviation for Extended Unique Identifier.
")
    (description
      "This package provides a library to generate and parse IEEE EUI-48 and EUI-64, also known as MAC-48 media access
control addresses.  The IEEE claims trademarks on the names EUI-48 and EUI-64, in which EUI is an
abbreviation for Extended Unique Identifier.
")
    (license (list license:expat license:asl2.0))))

(define rust-bit-vec-0.6
  (package
    (name "rust-bit-vec")
    (version "0.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bit-vec" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1ywqjnv60cdh1slhz67psnp422md6jdliji6alq0gmly2xm9p7rl"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-serde" ,rust-serde-1))))
    (home-page
      "https://github.com/contain-rs/bit-vec")
    (synopsis "A vector of bits")
    (description
      "This package provides a vector of bits")
    (license (list license:expat license:asl2.0))))

(define rust-postgres-types-0.2
  (package
    (name "rust-postgres-types")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "postgres-types" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0brsqkydz0grfy60nc1d0hxa9jbpim0c7c52v467nrdpw4ql23s3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bit-vec" ,rust-bit-vec-0.6)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-chrono" ,rust-chrono-0.4)
         ("rust-eui48" ,rust-eui48-0.4)
         ("rust-fallible-iterator"
          ,rust-fallible-iterator-0.2)
         ("rust-geo-types" ,rust-geo-types-0.7)
         ("rust-geo-types" ,rust-geo-types-0.6)
         ("rust-postgres-derive"
          ,rust-postgres-derive-0.4)
         ("rust-postgres-protocol"
          ,rust-postgres-protocol-0.6)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-time" ,rust-time-0.2)
         ("rust-uuid" ,rust-uuid-0.8))))
    (home-page
      "https://github.com/sfackler/rust-postgres")
    (synopsis
      "Conversions between Rust and Postgres values")
    (description
      "Conversions between Rust and Postgres values")
    (license (list license:expat license:asl2.0))))

(define rust-hmac-0.10
  (package
    (name "rust-hmac")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hmac" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "058yxq54x7xn0gk2vy9bl51r32c9z7qlcl2b80bjh3lk3rmiqi61"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-crypto-mac" ,rust-crypto-mac-0.10)
         ("rust-digest" ,rust-digest-0.9))))
    (home-page "https://github.com/RustCrypto/MACs")
    (synopsis
      "Generic implementation of Hash-based Message Authentication Code (HMAC)")
    (description
      "Generic implementation of Hash-based Message Authentication Code (HMAC)")
    (license (list license:expat license:asl2.0))))

(define rust-postgres-protocol-0.6
  (package
    (name "rust-postgres-protocol")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "postgres-protocol" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1wxzs78zvz00bh3bhbbp9hnq9hg77f8h5pzjmcy9481fsdq0ygpz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-base64" ,rust-base64-0.13)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-fallible-iterator"
          ,rust-fallible-iterator-0.2)
         ("rust-hmac" ,rust-hmac-0.10)
         ("rust-md-5" ,rust-md-5-0.9)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-sha2" ,rust-sha2-0.9)
         ("rust-stringprep" ,rust-stringprep-0.1))))
    (home-page
      "https://github.com/sfackler/rust-postgres")
    (synopsis "Low level Postgres protocol APIs")
    (description "Low level Postgres protocol APIs")
    (license (list license:expat license:asl2.0))))

(define rust-tokio-postgres-0.7
  (package
    (name "rust-tokio-postgres")
    (version "0.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-postgres" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "12rb390i3af7zb0z2idhaf6l2m6snypwdiwjw84rmyz4qy1i6ard"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-async-trait" ,rust-async-trait-0.1)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-fallible-iterator"
          ,rust-fallible-iterator-0.2)
         ("rust-futures" ,rust-futures-0.3)
         ("rust-log" ,rust-log-0.4)
         ("rust-parking-lot" ,rust-parking-lot-0.11)
         ("rust-percent-encoding"
          ,rust-percent-encoding-2)
         ("rust-phf" ,rust-phf-0.8)
         ("rust-pin-project-lite"
          ,rust-pin-project-lite-0.2)
         ("rust-postgres-protocol"
          ,rust-postgres-protocol-0.6)
         ("rust-postgres-types" ,rust-postgres-types-0.2)
         ("rust-socket2" ,rust-socket2-0.4)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tokio-util" ,rust-tokio-util-0.6))))
    (home-page
      "https://github.com/sfackler/rust-postgres")
    (synopsis
      "A native, asynchronous PostgreSQL client")
    (description
      "This package provides a native, asynchronous PostgreSQL client")
    (license (list license:expat license:asl2.0))))

(define rust-postgres-0.19
  (package
    (name "rust-postgres")
    (version "0.19.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "postgres" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1hnid1d78zrr8ph12lpvp5b2cpx2fsqqgqs2yn1q23c6g7jix1y7"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bytes" ,rust-bytes-1)
         ("rust-fallible-iterator"
          ,rust-fallible-iterator-0.2)
         ("rust-futures" ,rust-futures-0.3)
         ("rust-log" ,rust-log-0.4)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-tokio-postgres" ,rust-tokio-postgres-0.7))))
    (home-page
      "https://github.com/sfackler/rust-postgres")
    (synopsis
      "A native, synchronous PostgreSQL client")
    (description
      "This package provides a native, synchronous PostgreSQL client")
    (license (list license:expat license:asl2.0))))

(define rust-rust-decimal-1
  (package
    (name "rust-rust-decimal")
    (version "1.14.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rust_decimal" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1k7vwq8jqp8prjzg3ky03knd6wnva8k3jk7n787ml37wf8iyd1wp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-arbitrary" ,rust-arbitrary-1)
         ("rust-arrayvec" ,rust-arrayvec-0.5)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-bytes" ,rust-bytes-1)
         ("rust-diesel" ,rust-diesel-1)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-postgres" ,rust-postgres-0.19)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-tokio-postgres" ,rust-tokio-postgres-0.7))))
    (home-page
      "https://github.com/paupino/rust-decimal")
    (synopsis
      "A Decimal Implementation written in pure Rust suitable for financial calculations.")
    (description
      "This package provides a Decimal Implementation written in pure Rust suitable for financial calculations.")
    (license license:expat)))

(define rust-rhai-codegen-0.3
  (package
    (name "rust-rhai-codegen")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rhai_codegen" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1z6arm0w14kx9kiznhqzdb6a12y57lcf9brmwfllbzda35yxcgv4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1))))
    (home-page
      "https://rhai.rs/book/plugins/index.html")
    (synopsis
      "Procedural macros support package for Rhai, a scripting language and engine for Rust")
    (description
      "Procedural macros support package for Rhai, a scripting language and engine for Rust")
    (license (list license:expat license:asl2.0))))

(define rust-hashbrown-0.8
  (package
    (name "rust-hashbrown")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hashbrown" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "09cckr5l71ypvfdbvv1qsag4222blixwn9300hpbr831j3vn46z9"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-ahash" ,rust-ahash-0.3)
         ("rust-autocfg" ,rust-autocfg-1)
         ("rust-compiler-builtins"
          ,rust-compiler-builtins-0.1)
         ("rust-rayon" ,rust-rayon-1)
         ("rust-rustc-std-workspace-alloc"
          ,rust-rustc-std-workspace-alloc-1)
         ("rust-rustc-std-workspace-core"
          ,rust-rustc-std-workspace-core-1)
         ("rust-serde" ,rust-serde-1))))
    (home-page
      "https://github.com/rust-lang/hashbrown")
    (synopsis
      "A Rust port of Google's SwissTable hash map")
    (description
      "This package provides a Rust port of Google's SwissTable hash map")
    (license (list license:asl2.0 license:expat))))

(define rust-no-std-compat-0.4
  (package
    (name "rust-no-std-compat")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "no-std-compat" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "132vrf710zsdp40yp1z3kgc2ss8pi0z4gmihsz3y7hl4dpd56f5r"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-hashbrown" ,rust-hashbrown-0.8)
         ("rust-spin" ,rust-spin-0.5))))
    (home-page
      "https://gitlab.com/jD91mZM2/no-std-compat")
    (synopsis
      "A `#![no_std]` compatibility layer that will make porting your crate to no_std *easy*.")
    (description
      "This package provides a `#![no_std]` compatibility layer that will make porting your crate to no_std *easy*.")
    (license license:expat)))

(define rust-core-error-0.0.0
  (package
    (name "rust-core-error")
    (version "0.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "core-error" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13wvc7lcpi7f6rr0racns4l52gzpix4xhih6qns30hmn5sbv5kgg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-version-check" ,rust-version-check-0.9))))
    (home-page
      "https://github.com/core-error/core-error")
    (synopsis "std::error::Error for libcore")
    (description "std::error::Error for libcore")
    (license (list license:expat license:asl2.0))))

(define rust-crunchy-0.2
  (package
    (name "rust-crunchy")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crunchy" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dx9mypwd5mpfbbajm78xcrg5lirqk7934ik980mmaffg3hdm0bs"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "")
    (synopsis
      "Crunchy unroller: deterministically unroll constant loops")
    (description
      "Crunchy unroller: deterministically unroll constant loops")
    (license license:expat)))

(define rust-tiny-keccak-2
  (package
    (name "rust-tiny-keccak")
    (version "2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tiny-keccak" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0dq2x0hjffmixgyf6xv9wgsbcxkd65ld0wrfqmagji8a829kg79c"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-crunchy" ,rust-crunchy-0.2))))
    (home-page
      "https://github.com/debris/tiny-keccak")
    (synopsis
      "An implementation of Keccak derived functions.")
    (description
      "An implementation of Keccak derived functions.")
    (license license:cc0)))

(define rust-const-random-macro-0.1
  (package
    (name "rust-const-random-macro")
    (version "0.1.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "const-random-macro" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0h7vvskw1pw5x44sbl74gsi8ydvrj5kaixpjqzxvz8h0s0knwpv1"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-getrandom" ,rust-getrandom-0.2)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-proc-macro-hack"
          ,rust-proc-macro-hack-0.5)
         ("rust-tiny-keccak" ,rust-tiny-keccak-2))))
    (home-page
      "https://github.com/tkaitchuck/constrandom")
    (synopsis
      "Provides the procedural macro used by const-random")
    (description
      "This package provides the procedural macro used by const-random")
    (license (list license:expat license:asl2.0))))

(define rust-const-random-0.1
  (package
    (name "rust-const-random")
    (version "0.1.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "const-random" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1i3pmhmmcdw3rr1pv1p9yhm4danm5r156cpy7w30pa0s05fxk47m"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-const-random-macro"
          ,rust-const-random-macro-0.1)
         ("rust-proc-macro-hack"
          ,rust-proc-macro-hack-0.5))))
    (home-page
      "https://github.com/tkaitchuck/constrandom")
    (synopsis
      "Provides compile time random number generation.")
    (description
      "This package provides compile time random number generation.")
    (license (list license:expat license:asl2.0))))

(define rust-ahash-0.7
  (package
    (name "rust-ahash")
    (version "0.7.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ahash" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "163vy6jcd7r3jczsv4zyhlc5x9dqsfgg1yrqbm3xhygr1czq7fs3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-const-random" ,rust-const-random-0.1)
         ("rust-getrandom" ,rust-getrandom-0.2)
         ("rust-once-cell" ,rust-once-cell-1)
         ("rust-serde" ,rust-serde-1)
         ("rust-version-check" ,rust-version-check-0.9))))
    (home-page "https://github.com/tkaitchuck/ahash")
    (synopsis
      "A non-cryptographic hash function using AES-NI for high performance")
    (description
      "This package provides a non-cryptographic hash function using AES-NI for high performance")
    (license (list license:expat license:asl2.0))))

(define rust-rhai-0.20
  (package
    (name "rust-rhai")
    (version "0.20.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rhai" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1kivwx7ljx89ay7khsji75m0l084jk5vk21y0fjx4x3f4cjy2qnq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-ahash" ,rust-ahash-0.7)
         ("rust-core-error" ,rust-core-error-0.0.0)
         ("rust-instant" ,rust-instant-0.1)
         ("rust-libm" ,rust-libm-0.2)
         ("rust-no-std-compat" ,rust-no-std-compat-0.4)
         ("rust-num-traits" ,rust-num-traits-0.2)
         ("rust-rhai-codegen" ,rust-rhai-codegen-0.3)
         ("rust-rust-decimal" ,rust-rust-decimal-1)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-smallvec" ,rust-smallvec-1)
         ("rust-smartstring" ,rust-smartstring-0.2)
         ("rust-unicode-xid" ,rust-unicode-xid-0.2))))
    (home-page "https://rhai.rs")
    (synopsis "Embedded scripting for Rust")
    (description "Embedded scripting for Rust")
    (license (list license:expat license:asl2.0))))

(define rust-quick-error-2
  (package
    (name "rust-quick-error")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quick-error" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "18z6r2rcjvvf8cn92xjhm2qc3jpd1ljvcbf12zv0k9p565gmb4x9"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "http://github.com/tailhook/quick-error")
    (synopsis
      "    A macro which makes error types pleasant to write.
")
    (description
      "    A macro which makes error types pleasant to write.
")
    (license (list license:expat license:asl2.0))))

(define rust-handlebars-4
  (package
    (name "rust-handlebars")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "handlebars" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0n852lqkzm6sj77lx95qcv5k9pvrds7hl0r9jwn7xx7ymy4zxw18"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-log" ,rust-log-0.4)
         ("rust-pest" ,rust-pest-2)
         ("rust-pest-derive" ,rust-pest-derive-2)
         ("rust-quick-error" ,rust-quick-error-2)
         ("rust-rhai" ,rust-rhai-0.20)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-walkdir" ,rust-walkdir-2))))
    (home-page
      "https://github.com/sunng87/handlebars-rust")
    (synopsis
      "Handlebars templating implemented in Rust.")
    (description
      "Handlebars templating implemented in Rust.")
    (license license:expat)))

(define rust-gitignore-1
  (package
    (name "rust-gitignore")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gitignore" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "197rlas99iqc95fhikyspfa5flhva9pbl1jc8fn9h50ccbj91akq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-glob" ,rust-glob-0.3))))
    (home-page
      "https://github.com/nathankleyn/gitignore.rs")
    (synopsis
      "Implementation of .gitignore file parsing and glob testing in Rust.")
    (description
      "Implementation of .gitignore file parsing and glob testing in Rust.")
    (license (list license:expat license:asl2.0))))

(define rust-rust-stemmers-1
  (package
    (name "rust-rust-stemmers")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rust-stemmers" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0m6acgdflrrcm17dj7lp7x4sfqqhga24qynv660qinwz04v20sp4"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-serde" ,rust-serde-1)
         ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page
      "https://github.com/CurrySoftware/rust-stemmers")
    (synopsis
      "A rust implementation of some popular snowball stemming algorithms")
    (description
      "This package provides a rust implementation of some popular snowball stemming algorithms")
    (license (list license:expat license:bsd-3))))

(define rust-lindera-ipadic-0.3
  (package
    (name "rust-lindera-ipadic")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lindera-ipadic" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "04p1xipvalbhndllrr70qbv1ypyfj452yfl6y0glqvss716g84lz"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bincode" ,rust-bincode-1)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-lindera-core" ,rust-lindera-core-0.3))))
    (home-page
      "https://github.com/lindera-morphology/lindera")
    (synopsis
      "A Japanese morphological dictionary loader for IPADIC.")
    (description
      "This package provides a Japanese morphological dictionary loader for IPADIC.")
    (license license:expat)))

(define rust-lindera-dictionary-0.3
  (package
    (name "rust-lindera-dictionary")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lindera-dictionary" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1v5830982nani68q3jslbj64d8sg4wrncn23n2m4zljpc031m9kq"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bincode" ,rust-bincode-1)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-lindera-core" ,rust-lindera-core-0.3))))
    (home-page
      "https://github.com/lindera-morphology/lindera")
    (synopsis "A morphological dictionary loader.")
    (description
      "This package provides a morphological dictionary loader.")
    (license license:expat)))

(define rust-fst-0.3
  (package
    (name "rust-fst")
    (version "0.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fst" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1mpby7wa5mkpgjiilam94a2l9mxx9wpgs3nw2nr1a0czzwsb8zwj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1)
         ("rust-memmap" ,rust-memmap-0.6))))
    (home-page "https://github.com/BurntSushi/fst")
    (synopsis
      "Use finite state transducers to compactly represents sets or maps of many
strings (> 1 billion is possible).
")
    (description
      "Use finite state transducers to compactly represents sets or maps of many
strings (> 1 billion is possible).
")
    (license (list license:unlicense license:expat))))

(define rust-levenshtein-automata-0.1
  (package
    (name "rust-levenshtein-automata")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "levenshtein_automata" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1jp8yb4dyqxidp1id16vpgxdhjd2ars9gi0ain6m8s7lfzw0983k"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-fst" ,rust-fst-0.3))))
    (home-page
      "https://github.com/tantivy-search/levenshtein-automata")
    (synopsis
      "Creates Levenshtein Automata in an efficient manner.")
    (description
      "Creates Levenshtein Automata in an efficient manner.")
    (license license:expat)))

(define rust-lindera-fst-0.1
  (package
    (name "rust-lindera-fst")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lindera-fst" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1q2a52q0m9y8pr2wi5434z2m41cr5wizlzi25p6rd4k7lry8l2d6"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-byteorder" ,rust-byteorder-1)
         ("rust-levenshtein-automata"
          ,rust-levenshtein-automata-0.1)
         ("rust-regex-syntax" ,rust-regex-syntax-0.4)
         ("rust-utf8-ranges" ,rust-utf8-ranges-1))))
    (home-page
      "https://github.com/lindera-morphology/lindera-fst")
    (synopsis
      "This is a lindera-specific fork from the fst crate from Burntsushi. (Please use the fst crate instead.)")
    (description
      "This is a lindera-specific fork from the fst crate from Burntsushi. (Please use the fst crate instead.)")
    (license (list license:unlicense license:expat))))

(define rust-lindera-core-0.3
  (package
    (name "rust-lindera-core")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lindera-core" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1zw1nrdm54zpnjd4qknzxdkqiywp41ilqhr8dciw2qfkllrg3dwp"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bincode" ,rust-bincode-1)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-encoding" ,rust-encoding-0.2)
         ("rust-lindera-fst" ,rust-lindera-fst-0.1)
         ("rust-serde" ,rust-serde-1))))
    (home-page
      "https://github.com/lindera-morphology/lindera")
    (synopsis
      "A morphological analysis core library.")
    (description
      "This package provides a morphological analysis core library.")
    (license license:expat)))

(define rust-lindera-0.3
  (package
    (name "rust-lindera")
    (version "0.3.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lindera" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "004c29dbmdhlv9q4n123ya2j9vamp9m1ldmqv2k1kz7md36ngf3i"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-bincode" ,rust-bincode-1)
         ("rust-byteorder" ,rust-byteorder-1)
         ("rust-encoding" ,rust-encoding-0.2)
         ("rust-lindera-core" ,rust-lindera-core-0.3)
         ("rust-lindera-dictionary"
          ,rust-lindera-dictionary-0.3)
         ("rust-lindera-fst" ,rust-lindera-fst-0.1)
         ("rust-lindera-ipadic" ,rust-lindera-ipadic-0.3)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1))))
    (home-page
      "https://github.com/lindera-morphology/lindera")
    (synopsis "A morphological analysis library.")
    (description
      "This package provides a morphological analysis library.")
    (license license:expat)))

(define rust-cedarwood-0.4
  (package
    (name "rust-cedarwood")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cedarwood" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "13glk5w8hndgy553nrdmqxdvlr0b4s6n0lm4lf680qs1p73q4gln"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-smallvec" ,rust-smallvec-1))))
    (home-page "https://github.com/MnO2/cedarwood")
    (synopsis
      "efficiently-updatable double-array trie in Rust (ported from cedar)")
    (description
      "efficiently-updatable double-array trie in Rust (ported from cedar)")
    (license #f)))

(define rust-jieba-rs-0.5
  (package
    (name "rust-jieba-rs")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jieba-rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0z0k1n38s8x0h0rz08rrv41lv9rmmvvl55bxj5h78wlk7rrdx8ic"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-cedarwood" ,rust-cedarwood-0.4)
         ("rust-hashbrown" ,rust-hashbrown-0.8)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-phf" ,rust-phf-0.8)
         ("rust-phf-codegen" ,rust-phf-codegen-0.8)
         ("rust-regex" ,rust-regex-1))))
    (home-page
      "https://github.com/messense/jieba-rs")
    (synopsis
      "The Jieba Chinese Word Segmentation Implemented in Rust")
    (description
      "The Jieba Chinese Word Segmentation Implemented in Rust")
    (license license:expat)))

(define rust-elasticlunr-rs-2
  (package
    (name "rust-elasticlunr-rs")
    (version "2.3.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "elasticlunr-rs" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0066sskvmmmmjvahg5sn6c2qng5fq4pmlx7m89lwxbm734xzg31g"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-jieba-rs" ,rust-jieba-rs-0.5)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-lindera" ,rust-lindera-0.3)
         ("rust-regex" ,rust-regex-1)
         ("rust-rust-stemmers" ,rust-rust-stemmers-1)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-derive" ,rust-serde-derive-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-strum" ,rust-strum-0.18)
         ("rust-strum-macros" ,rust-strum-macros-0.18))))
    (home-page
      "https://github.com/mattico/elasticlunr-rs")
    (synopsis
      "A partial port of elasticlunr.js to Rust for generating static document search indexes")
    (description
      "This package provides a partial port of elasticlunr.js to Rust for generating static document search indexes")
    (license (list license:expat license:asl2.0))))

(define rust-markup5ever-rcdom-0.1
  (package
    (name "rust-markup5ever-rcdom")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "markup5ever_rcdom" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0axf6vrms8579lvhbjaj0v7bhs8xb7s26d4sam2g3m6qpi1xl5gh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-html5ever" ,rust-html5ever-0.25)
         ("rust-markup5ever" ,rust-markup5ever-0.10)
         ("rust-tendril" ,rust-tendril-0.4)
         ("rust-xml5ever" ,rust-xml5ever-0.16))))
    (home-page "https://github.com/servo/html5ever")
    (synopsis
      "Basic, unsupported DOM structure for use by tests in html5ever/xml5ever")
    (description
      "Basic, unsupported DOM structure for use by tests in html5ever/xml5ever")
    (license (list license:expat license:asl2.0))))

(define rust-ammonia-3
  (package
    (name "rust-ammonia")
    (version "3.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ammonia" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0yb25xrb7bn1s7hchqvpxcv6vhyby0bxypf9xmf7qcvz2pmxdrqy"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build?
        #t
        #:cargo-inputs
        (("rust-html5ever" ,rust-html5ever-0.25)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-maplit" ,rust-maplit-1)
         ("rust-markup5ever-rcdom"
          ,rust-markup5ever-rcdom-0.1)
         ("rust-matches" ,rust-matches-0.1)
         ("rust-tendril" ,rust-tendril-0.4)
         ("rust-url" ,rust-url-2))))
    (home-page
      "https://github.com/rust-ammonia/ammonia")
    (synopsis "HTML Sanitization")
    (description "HTML Sanitization")
    (license (list license:expat license:asl2.0))))

(define rust-mdbook-0.4
  (package
    (name "rust-mdbook")
    (version "0.4.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mdBook" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "11gslsrfy7yszsg6klfkfk73szhihwdxjrr3skhagm70kmh0xnmn"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-ammonia" ,rust-ammonia-3)
         ("rust-anyhow" ,rust-anyhow-1)
         ("rust-chrono" ,rust-chrono-0.4)
         ("rust-clap" ,rust-clap-2)
         ("rust-elasticlunr-rs" ,rust-elasticlunr-rs-2)
         ("rust-env-logger" ,rust-env-logger-0.7)
         ("rust-futures-util" ,rust-futures-util-0.3)
         ("rust-gitignore" ,rust-gitignore-1)
         ("rust-handlebars" ,rust-handlebars-4)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-log" ,rust-log-0.4)
         ("rust-memchr" ,rust-memchr-2)
         ("rust-notify" ,rust-notify-4)
         ("rust-open" ,rust-open-1)
         ("rust-pulldown-cmark" ,rust-pulldown-cmark-0.7)
         ("rust-regex" ,rust-regex-1)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-derive" ,rust-serde-derive-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-shlex" ,rust-shlex-1)
         ("rust-tempfile" ,rust-tempfile-3)
         ("rust-tokio" ,rust-tokio-0.2)
         ("rust-toml" ,rust-toml-0.5)
         ("rust-warp" ,rust-warp-0.2))
        #:cargo-development-inputs
        (("rust-pretty-assertions"
          ,rust-pretty-assertions-0.6)
         ("rust-select" ,rust-select-0.5)
         ("rust-semver" ,rust-semver-0.11)
         ("rust-walkdir" ,rust-walkdir-2))
        #:tests? #f))
    (home-page "https://github.com/rust-lang/mdBook")
    (synopsis "Creates a book from markdown files")
    (description
      "Creates a book from markdown files")
    (license license:mpl2.0)))

