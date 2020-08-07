final: prev: {
  nix-index = final.rustPlatform.buildRustPackage rec {
    inherit (prev.nix-index.drvAttrs) pname nativeBuildInputs buildInputs doCheck postInstall;
    inherit (prev.nix-index) meta;
    version = "${final.lib.substring 0 7 src.rev}-patched";
    src = final.fetchFromGitHub {
      owner = "bennofs";
      repo = "nix-index";
      rev = "1ed54f7504a04ce179e55bb32622f91622b5d6b9";
      sha256 = "0azx6gwynjvck4fa7zb5rikmqys4bmgk46gdics8d1bsibbpp831";
    };
    cargoSha256 = "14dyc4p861pnjmindwrbz3q384av5mg7600scqvj1w96phhg679c";
    patches = [
      (final.writeText "hydra.patch" ''
diff --git a/src/hydra.rs b/src/hydra.rs
index 1cde305..21d7b06 100644
--- a/src/hydra.rs
+++ b/src/hydra.rs
@@ -342,8 +342,8 @@ impl Fetcher {
         &'a self,
         path: &StorePath,
     ) -> Box<dyn Future<Item = Option<FileTree>, Error = Error> + 'a> {
-        let url_xz = format!("{}/{}.ls.xz", self.cache_url, path.hash());
-        let url_generic = format!("{}/{}.ls", self.cache_url, path.hash());
+        let url_xz = format!("{}/{}-{}.ls.xz", self.cache_url, path.hash(), path.name());
+        let url_generic = format!("{}/{}-{}.ls", self.cache_url, path.hash(), path.name());
         let name = format!("{}.json", path.hash());

         let fetched = self.fetch(url_generic, None).and_then(
      '')
    ];
  };
}
