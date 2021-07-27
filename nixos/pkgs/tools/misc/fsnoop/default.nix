{ lib, stdenv, withSources, linuxHeaders, fetchurl }:

let
  headers =
    (linuxHeaders.overrideAttrs (old: let version = "5.1.21"; in {
      inherit version;
      src = fetchurl {
        url = "mirror://kernel/linux/kernel/v5.x/linux-${version}.tar.xz";
        sha256 = "1xj1wfhjz2s5a8j6zx3fsd7rrrkvw5waszzylf2gn3ag6615yjan";
      };
      # We don't need the patches for compiling fsnoop, and as of writing,
      # they don't work for the newer kernel version we need.
      patches = [];
    }));
in

stdenv.mkDerivation rec {
  pname = "fsnoop";
  rev = "e05bfc949838e094161360ce5a046cdaa097d39a";
  version = "git-${src.shortRev}";

  src = withSources.fsnoop;

  buildInputs = [
    # TODO: Remove the override when `linuxHeaders` is a version >= 5.1 to
    #       have `FAN_CREATE`; as of writing, it is 4.19 even on NixOS 20.03.
    headers
  ];

  installPhase = ''
    mkdir -p $out/bin/
    cp ./fsnoop $out/bin/
  '';

  meta = with lib; {
    description = "Dumps all filesystem events for a specific mount using the Linux fanotify interface";
    homepage = https://github.com/jeffwalter/fsnoop;
    license = licenses.mit;
    maintainers = [ maintainers.nh2 ];
    platforms = platforms.linux;
  };
}
