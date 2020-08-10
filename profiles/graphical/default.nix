{ config, lib, pkgs, ... }:

{
  imports = [
    ./xkb
    ./exwm
   #./xmonad
    ./lxqt
   #./qutebrowser
    ../develop
  ];

  environment.systemPackages = with pkgs; [
    velox
  ];

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.pulseaudio.enable = lib.mkDefault true;

  services.xserver = {
    enable = true;

    displayManager = {
      sddm = {
        enable = true;
        theme = "chili";
        autoLogin.relogin = true;
        extraConfig = ''
          [X11]
          UserAuthFile=.local/share/sddm/Xauthority
        '';
      };
      autoLogin = {
        enable = true;
        user = "bao";
      };
      defaultSession = "none+exwm";
    };
  };

  fonts.fontconfig.localConf = ''
    <?xml version="1.0"?>
    <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
    <fontconfig>
      <!-- Priority:
           1. The generic family OR specific family
           2. The emoji font family (defined in 60-generic.conf)
           3. All the rest
      -->
      <alias binding="weak">
        <family>monospace</family>
        <prefer>
          <family>emoji</family>
        </prefer>
      </alias>
      <alias binding="weak">
        <family>sans-serif</family>
        <prefer>
          <family>emoji</family>
        </prefer>
      </alias>

      <alias binding="weak">
        <family>serif</family>
        <prefer>
          <family>emoji</family>
        </prefer>
      </alias>

      <selectfont>
        <rejectfont>
          <!-- Reject DejaVu fonts, they interfere with color emoji. -->
          <pattern>
            <patelt name="family">
              <string>DejaVu Sans</string>
            </patelt>
          </pattern>
          <pattern>
            <patelt name="family">
              <string>DejaVu Serif</string>
            </patelt>
          </pattern>
          <pattern>
            <patelt name="family">
              <string>DejaVu Sans Mono</string>
            </patelt>
          </pattern>

          <!-- Also reject EmojiOne Mozilla and Twemoji Mozilla; I want Noto Color Emoji -->
          <pattern>
            <patelt name="family">
              <string>EmojiOne Mozilla</string>
            </patelt>
          </pattern>
          <pattern>
            <patelt name="family">
              <string>Twemoji Mozilla</string>
            </patelt>
          </pattern>
        </rejectfont>
      </selectfont>
    </fontconfig>
  '';
}
