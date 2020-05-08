{ config, lib, pkgs, ... }:

let
  ladspaPath = "${pkgs.ladspaPlugins}/lib/ladspa";

  sinks = {
    #hdmi = "alsa_output.pci-0000_01_00.1.hdmi-stereo-extra3";
    stereo = "alsa_output.pci-0000_00_1b.0.analog-stereo";
    nc-25 = "bluez_sink.11_11_22_33_33_98.a2dp_sink";
  };

  # ladspa sink queues
  # ------------------
  # generates a chain of effects. Usefull for limiting the output of movies
  # queue here starts from the audio input and should end at the output
  #
  # example for audio -> a -> b -> c -> default_output :
  #   queue = [ a b c ]
  #
  # queue element :
  # {
  #   # name of the .so file
  #   plugin  = "dyson_compress_1403";
  #   # label of the plugin (must be correct)
  #   label   = "dysonCompress";
  #   # control parameters of the plugin
  #   control = [ "0" "1" "0.2" "0.8"];
  # }
  #
  # have a look at : http://plugin.org.uk/ladspa-swh/docs/ladspa-swh.html
  ladspaSinkQueues = [
    {
      name = "mediaLimiterSink";
      queue = [
        #3 compress all sounds
        {
          plugin  = "dyson_compress_1403";
          label   = "dysonCompress";
          control = [
            "0"    # peak limit (dB)
            "1"    # release time (secons)
            "0.2"  # fast compression ration (unknown what that means)
            "0.8"  # compression ratio
          ];
        }
        #2 limit sound (normalize)
        {
          plugin  = "fast_lookahead_limiter_1913";
          label   = "fastLookaheadLimiter";
          control = [
            "20"   # input gain (db)
            "-10"  # limit (db)
            "1.1"  # release time (s)
          ];
        }
        #1 avoid deep sounds
        {
          plugin  = "dj_eq_1901";
          label   = "dj_eq";
          control = [
            "-9"  # low  gain (db) (100Hz)
            "0"   # mid  gain (db) (1000Hz)
            "0"   # high gain (db) (10000Hz)
          ];
        }
      ];
    }
  ];
in {

  # add virtual midi module
  # -----------------------
  boot = {
    # to route midi signals
    # between bitwig and vcvrack
    kernelModules = [ "snd_virmidi" ];
    # index=-2  prevents from beeing recognised as the default
    #           audio device
    # midi_devs limit the number of midi devices.
    extraModprobeConfig = "options snd-virmidi index=-2 midi_devs=1";
  };

  # LADSPA
  # ------

  environment.variables = {
    # set ladspa library path
    # about testing the plugins check analyseplugin command
    LADSPA_PATH = "${ladspaPath}";
  };

  # PulseAudio
  # ----------
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    extraModules = with pkgs; [ pulseaudio-modules-bt ];
    zeroconf.discovery.enable = true;
    zeroconf.publish.enable = true;
    tcp.enable = true;
   #package = with pkgs; pulseaudioFull.override {
   #  jackaudioSupport = false;
   #};
    extraConfig = let
      inherit (builtins) toString;
    in ''
      # automatically switch to newly-connected devices
      load-module module-switch-on-connect

      ${lib.concatMapStrings (queue: ''
        # ladspa queue - ${queue.name}
        ${lib.concatImapStrings (index: config: let
          sinkName     = suffix: "${queue.name}${toString suffix}";
          sinkValue    = "sink_name=${sinkName index}";
          masterValue  = if (index == 1) then "" else "master=${sinkName (index - 1)}";
          pluginValue  = "plugin=${ladspaPath}/${config.plugin}";
          labelValue   = "label=${config.label}";
          controlValue = "control=${toString (lib.foldl (a: b: "${a},${b}") (lib.head config.control) (lib.tail config.control))}";
        in ''
          # ${sinkName index} (${toString index}) - ${config.label}
          load-module module-ladspa-sink ${sinkValue} ${masterValue} ${pluginValue} ${labelValue} ${controlValue}
        '') (lib.reverseList queue.queue)}
      '' ) ladspaSinkQueues}

      set-default-sink mediaLimiterSink3
    '';
  };

  # Packages needed
  # ---------------
  environment.systemPackages = with pkgs; [

    # ALSA Tools
    # ------
    alsaUtils

    # LADSPA
    # ------
    ladspaPlugins
    ladspa-sdk

    # PulseAudio control
    # ------------------
    ncpamixer
    pavucontrol
    pulseeffects
    lxqt.pavucontrol-qt
    pulseaudio-ctl
    pasystray
  ];

}
