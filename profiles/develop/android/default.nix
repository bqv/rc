{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    androidenv.androidPkgs_9_0.androidsdk
    androidenv.androidPkgs_9_0.platform-tools
    jre (pkgs.lowPrio graalvm8-ce) (pkgs.lowPrio e2fsprogs)
    jadx apktool
  ];
}
