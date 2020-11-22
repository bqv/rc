{ stdenv, flake, openssl, pkgconfig, rustPlatform }:

stdenv.mkDerivation rec {
  name = "pure-${version}";
  version = flake.inputs.zsh-pure.shortRev;

  srcs = flake.inputs.zsh-pure;

  buildPhase = "true";

  installPhase = ''
    mkdir -p $out/share/zsh/plugins/pure
    cp -r ./ $out/share/zsh/plugins/pure
  '';

  meta = with stdenv.lib; {
    description = "Pretty, minimal and fast ZSH prompt";
    homepage = "https://github.com/sindresorhus/pure";
    maintainers = [ maintainers.nrdxp ];
    license = licenses.mit;
    inherit version;
  };
}
