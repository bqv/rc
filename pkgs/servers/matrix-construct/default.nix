{ stdenv, lib, fetchFromGitHub, autoreconfHook, pkg-config, llvmPackages_9
, libsodium, openssl, file, boost, rocksdb, gmp, llvm
, mkDerivation ? llvmPackages_9.stdenv.mkDerivation # Build Chain
, zlib, lz4, snappy # Database Compression
, graphicsmagick # Media Thumbnails
, jemalloc # Dynamic Memory
, debug ? false # Debug Build
, ... }:

mkDerivation rec {
  pname = "matrix-construct";
  version = "2020.04.07";

  src = fetchFromGitHub {
    owner = "jevolk";
    repo = "charybdis";
    rev = "7b1ca4964680e46141ed81fed07f7de5425a4754";
    hash = "sha256-/w4osy16w1VKgL6aZrgQBQmm5iQ+6V61bC4yCgqY5/0=";
  };

  configureFlags = [
    "--enable-generic"
    "--with-boost-libdir=${boost.out}/lib"
    "--with-boost=${boost.dev}"
    "--with-imagemagick-includes=${graphicsmagick}/include/GraphicsMagick"
    "--with-imagemagick-libs=${graphicsmagick}/lib"
  ] ++ lib.optional (!isNull jemalloc) "--enable-jemalloc"
    ++ lib.optional debug "--with-log-level=DEBUG";

  postConfigure = ''
    sed -i '/RB_CONF_DIR/s%^.*$%#define RB_CONF_DIR "/etc"%' include/ircd/config.h
    sed -i '/RB_DB_DIR/s%^.*$%#define RB_DB_DIR "/var/db/${pname}"%' include/ircd/config.h
    sed -i '/RB_LOG_DIR/s%^.*$%#define RB_LOG_DIR "/var/log/${pname}"%' include/ircd/config.h
    substituteInPlace ircd/magic.cc --replace "/usr/local/share/misc/magic.mgc" "${file}/share/misc/magic.mgc"
  '';

  cmakeFlags = [
    "-DWITH_TESTS=1"
    "-DWITH_TOOLS=1"
    "-DUSE_RTTI=1"
    "-DWITH_LZ4=1"
    "-DBUILD_SHARED_LIBS=1"
  ];

  nativeBuildInputs = [ autoreconfHook pkg-config ];
  buildInputs = [
    libsodium
    openssl
    file
    boost
    (rocksdb.overrideAttrs (super: rec {
      version = "5.16.6";
      src = fetchFromGitHub {
        owner = "facebook";
        repo = "rocksdb";
        rev = "v${version}";
        sha256 = "0yy09myzbi99qdmh2c2mxlddr12pwxzh66ym1y6raaqglrsmax66";
      };
      NIX_CFLAGS_COMPILE = "${super.NIX_CFLAGS_COMPILE} -Wno-error=redundant-move";
    }))
    zlib
    lz4
    graphicsmagick
    jemalloc
    gmp
    snappy
    llvm
  ];
}
