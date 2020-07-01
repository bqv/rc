{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    androidenv.androidPkgs_9_0.androidsdk
    androidenv.androidPkgs_9_0.platform-tools
    jre graalvm8
    jadx apktool
  ];
}
