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
    rev = "ea97994fe3939e226f2d7fc87838fe71c2e72fa3";
    hash = "sha256-RJlHk4pOclqo9yaS5v92BFpqDzY0dGQDJMzSN200sh4=";
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
    "--with-boost-libdir=${boost.out}/lib"
    "--with-boost=${boost.dev}"
    "--with-magic-file=${file}/share/misc/magic.mgc"
  ] ++ lib.optional useJemalloc "--enable-jemalloc"
    ++ lib.optional withGraphicsMagick [
    "--with-imagemagick-includes=${graphicsmagick}/include/GraphicsMagick"
  ] ++ lib.optional debug "--with-log-level=DEBUG";

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
      nativeBuildInputs = super.nativeBuildInputs ++ [ jemalloc ];
      cmakeFlags = builtins.map (f: if f == "-DWITH_JEMALLOC=0" then "-DWITH_JEMALLOC=1" else
                                    if f == "-DWITH_TOOLS=0" then "-DWITH_TOOLS=1" else f) super.cmakeFlags;
      NIX_CFLAGS_COMPILE = "${super.NIX_CFLAGS_COMPILE} -Wno-error=redundant-move";
    }))
    graphicsmagick
    jemalloc llvm
  ];

  doInstallCheck = true;
  installCheckPhase = ''
    chmod -R a-w $out
    mkdir -p /tmp/cache/construct
    export RUNTIME_DIRECTORY=/tmp/run/construct
    export STATE_DIRECTORY=/tmp/lib/construct
    export LOGS_DIRECTORY=/tmp/log/construct
    cd /tmp/cache/construct
    $out/bin/construct -smoketest localhost
  '';
}
