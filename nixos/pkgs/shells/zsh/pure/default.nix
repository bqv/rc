{ lib, stdenv, withSources, openssl, pkgconfig, rustPlatform }:

stdenv.mkDerivation rec {
  name = "pure-${version}";
  version = srcs.shortRev;

  srcs = withSources.zsh-pure;

  buildPhase = "true";

  installPhase = ''
    mkdir -p $out/share/zsh/plugins/pure
    cp -r ./ $out/share/zsh/plugins/pure
  '';

  meta = with lib; {
    description = "Pretty, minimal and fast ZSH prompt";
    homepage = "https://github.com/sindresorhus/pure";
    maintainers = [ maintainers.nrdxp ];
    license = licenses.mit;
    inherit version;
  };
}
