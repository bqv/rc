{ config, pkgs, lib, ... }:

let
  profiles = {
   #pulseaudio = ../apparmor/usr.bin.pulseaudio;
   #firefox = ../apparmor/usr.bin.firefox;
    chromium = ../apparmor/usr.bin.chrome;
    skype = ../apparmor/usr.bin.skypeforlinux;
    wine = ../apparmor/usr.bin.wine;
  };

  initScript = pkgs.writeShellScript "aa-init" ''
    enabled_conf=/etc/apparmor/enabled.conf
    profiles_dir=/etc/apparmor.d
    profiles_sysfs=/sys/kernel/security/apparmor/profiles
    cache_dir=/var/lib/apparmor
    err=0

    mkdir -p -m700 "$cache_dir"
    chown root: "$cache_dir"
    chmod 700 "$cache_dir"
    cache=( --cache-loc "$cache_dir" --write-cache )

    cd "$profiles_dir"
    while read p
    do apparmor_parser "''${cache[@]}" -r "$p" >&2 || err=1
    done < "$enabled_conf"

    if [[ -e "$profiles_sysfs" ]]; then
      enforce=()
      while read bin
      do enforce=( ''${enforce[@]} $(bash -c "ls -1d $bin" 2>/dev/null) ) # bash is for wildcards
      done < <(awk '!/\s+\(enforce\)$/ {print $1}' "$profiles_sysfs")
      for p in "''${enforce[@]}"; do aa-enforce "$p" >/dev/null; done

      awk '!/\s+\(enforce\)$/ {exit 1}' "$profiles_sysfs" \
        || { echo >&2 "ERROR: non-enforced profiles are detected"; exit 1; }
    fi

    exit $err
  '';
in {
  security.apparmor.policies = builtins.mapAttrs (n: p: {
    profile = ''
      #include ${p}
    '';
  }) profiles;
}
