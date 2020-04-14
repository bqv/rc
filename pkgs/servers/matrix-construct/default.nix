{ lib, fetchFromGitHub, autoreconfHook, pkg-config, llvmPackages_latest, gcc
, libsodium, openssl, file, boost, gmp, rocksdb

, llvm           ? if useClang then llvmPackages_latest.llvm else null
, graphicsmagick ? if withGraphicsMagick then graphicsmagick else null
, jemalloc       ? if useJemalloc then jemalloc else null
, stdenv         ? if useClang # Build Chain
                   then (if stdenv.cc.isClang then stdenv else llvmPackages_latest.stdenv)
                   else (if stdenv.cc.isGNU then stdenv else gcc.stdenv)

, debug              ? false # Debug Build
, useClang           ? false # Use Clang over GCC
, useJemalloc        ? true # Use the Dynamic Memory Allocator
, withGraphicsMagick ? true # Allow Media Thumbnails
, ... }:

stdenv.mkDerivation rec {
  pname = "matrix-construct";
  version = lib.substring 0 9 src.rev;

  src = fetchFromGitHub {
    owner = "jevolk";
    repo = "charybdis";
    rev = "868b515c6794d0f5f8aa2d307a169b12ed960a13";
    hash = "sha256-m0H+shmOY1/lcGV+3kkkv4pR7B2Z+jr4NIQSdsw4v0k=";
  };

  preAutoreconf = let
    VERSION_COMMIT_CMD="git rev-parse --short HEAD";
    VERSION_BRANCH_CMD="git rev-parse --abbrev-ref HEAD";
    VERSION_TAG_CMD="git describe --tags --abbrev=0 --dirty --always";
    VERSION_CMD="git describe --tags --always";
  in ''
    substituteInPlace configure.ac --replace "${VERSION_COMMIT_CMD}" "echo ${src.rev}"
    substituteInPlace configure.ac --replace "${VERSION_BRANCH_CMD}" "echo master"
    substituteInPlace configure.ac --replace "${VERSION_TAG_CMD}" "echo ${src.rev}"
    substituteInPlace configure.ac --replace "${VERSION_CMD}" "echo ${lib.substring 0 9 src.rev}"
  '';

  configureFlags = [
    "--enable-generic"
    "--with-custom-branding=nix"
    "--with-custom-version=${src.nixpkgsVersion}"
    "--with-boost-libdir=${boost.out}/lib"
    "--with-boost=${boost.dev}"
  ] ++ lib.optional useJemalloc "--enable-jemalloc"
    ++ lib.optional withGraphicsMagick [
    "--with-imagemagick-includes=${graphicsmagick}/include/GraphicsMagick"
  ] ++ lib.optional debug "--with-log-level=DEBUG";

  postConfigure = ''
    sed -i '/RB_CONF_DIR/s%^.*$%#define RB_CONF_DIR "/etc"%' include/ircd/config.h
    sed -i '/RB_DB_DIR/s%^.*$%#define RB_DB_DIR "/var/db/${pname}"%' include/ircd/config.h
    sed -i '/RB_LOG_DIR/s%^.*$%#define RB_LOG_DIR "/var/log/${pname}"%' include/ircd/config.h
    substituteInPlace ircd/magic.cc --replace "/usr/local/share/misc/magic.mgc" "${file}/share/misc/magic.mgc"
  '';

  enableParallelBuilding = true;

  nativeBuildInputs = [ autoreconfHook pkg-config ];
  buildInputs = [
    libsodium openssl file boost gmp
    (rocksdb.overrideAttrs (super: rec {
      version = "5.16.6";
      src = fetchFromGitHub {
        owner = "facebook"; repo = "rocksdb"; rev = "v${version}";
        sha256 = "0yy09myzbi99qdmh2c2mxlddr12pwxzh66ym1y6raaqglrsmax66";
      };
      NIX_CFLAGS_COMPILE = "${super.NIX_CFLAGS_COMPILE} -Wno-error=redundant-move";
    }))
    graphicsmagick
    jemalloc llvm
  ];
}
