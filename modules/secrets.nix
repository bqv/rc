{ lib, config, options, ... }:
let
  inherit (lib) types;
  # Ideas (inspiration nixops):
  # - Add systemd units for each key
  # - persistent/non-persistent keys, send keys after reboot

  # Abstract where the secret is gotten from (different hosts, not only localhost, different commands, not just files)

  # Note: This is persisted
  # Note: NixOS by default adds /run/keys as a ramfs with 750 permissions and group config.ids.gids.key
  keyDirectory = "/var/lib/nixus-secrets";

  secretType = pkgs: { name, config, ... }: {
    options = {
      file = lib.mkOption {
        type = types.path;
        apply = indirectSecret pkgs config name;
      };
      user = lib.mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          The owning user of the secret. If this is set, only that user can
          access the secret. Mutually exclusive with setting a group.
        '';
      };
      group = lib.mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          The owning group of the secret. If this is set, only that group can
          access the secret. Mutually exclusive with setting a user.
        '';
      };
    };
  };

  # Takes a file path and turns it into a derivation
  indirectSecret = pkgs: config: name: file: pkgs.runCommandNoCC "secret-${name}" {
    # To find out which file to copy. toString to not import the secret into
    # the store
    file = toString file;

    # We make this derivation dependent on the secret itself, such that a
    # change of it causes a rebuild
    secretHash = builtins.hashString "sha512" (builtins.readFile file);
    # TODO: Switch to `builtins.hashFile "sha512" value`
    # which requires Nix 2.3. The readFile way can cause an error when it
    # contains null bytes
  } (
    let
      validSecret = (config.user == null) != (config.group == null);
      subdir = if config.user != null
        then "per-user/${config.user}"
        else "per-group/${config.group}";
      target = if validSecret
        then "${keyDirectory}/active/${subdir}/${name}"
        else throw "nixus: secret.${name} needs to either have a user or a group set, but not both";
    in ''
      ln -s ${lib.escapeShellArg target} "$out"
    '');

  # Intersects the closure of a system with a set of secrets
  requiredSecrets = pkgs: { system, secrets }: pkgs.stdenv.mkDerivation {
    name = "required-secrets";

    __structuredAttrs = true;
    preferLocalBuild = true;

    exportReferencesGraph.system = system;
    secrets = lib.mapAttrsToList (name: value: {
      inherit name;
      path = value.file;
      source = value.file.file;
      hash = value.file.secretHash;
      inherit (value) user group;
    }) secrets;

    PATH = lib.makeBinPath [pkgs.buildPackages.jq];

    builder =
      let
        jqFilter = builtins.toFile "jq-filter" ''
          [.system[].path] as $system
          | .secrets[]
          | select(.path == $system[])
        '';
      in builtins.toFile "builder" ''
        source .attrs.sh
        jq -r -c -f ${jqFilter} .attrs.json > ''${outputs[out]}
      '';
  };

in {

  options.defaults = lib.mkOption {
    type = types.submodule ({ config, pkgs, ... }: {
      options.configuration = lib.mkOption {
        type = types.submoduleWith {
          modules = [{
            options.secrets = lib.mkOption {
              type = types.attrsOf (types.submodule (secretType pkgs));
              default = {};
            };
          }];
        };
      };

      # These scripts are intentionally not conditioned on secrets being defined
      # This is such that if a user enabled and disables secrets, they won't
      # stick around
      config =
        let
          includedSecrets = requiredSecrets pkgs {
            system = config.configuration.system.build.toplevel;
            secrets = config.configuration.secrets;
          };

        in {

        /*

        Secret structure:
        /var/lib/nixus-secrets/active  root:root    0755   # Directory containing all active persisted secrets and data needed to support it
          |
          + included-secrets           root:root    0750   # A file containing line-delimited json values describing all present secrets
          |
          + per-user                   root:root    0755   # A directory containing all secrets owned by users
          | |
          | + <user>                   <user>:root  0500   # A directory containing all secrets owned by <user>
          |   |                                            # Permissions are as restrictive as possible, some programs like ssh require this
          |   |
          |   + <name>                 <user>:root  0400   # A file containing the secret <name>
          |
          + per-group                  root:root    0755   # A directory containing all secrets owned by groups
            |
            + <group>                  root:<group> 0050   # A directory containing all secrets owned by <group>
              |
              + <name>                 root:<group> 0040   # A file containing the secret <name>

        /var/lib/nixus-secrets/pending root:root    0755   # The same structure as /active, but this is only used during deployment to make it more atomic and simple to remove unneeded ones later
                                                           # The only difference here is that no owners are set yet, since we can't yet know uid and gid
        */

        # Configures owners for secrets. This needs to happen during activation since only then the users/groups even exist
        # And this can't be done with tmpfiles.d because that would be part of the system closure, which is evaluated to even know which secrets to include
        # FIXME: This only works with rollbacks because nixus rolls back by essentially redeploying. So this will probably not work with e.g. grub rollbacks
        configuration.system.activationScripts.activate-secrets = lib.stringAfter [ "users" "groups" ] ''
          if [[ -d ${keyDirectory}/pending ]]; then
            while read -r json; do
              name=$(echo "$json" | ${pkgs.jq}/bin/jq -r '.name')
              user=$(echo "$json" | ${pkgs.jq}/bin/jq -r '.user')
              group=$(echo "$json" | ${pkgs.jq}/bin/jq -r '.group')

              # If this is a per-user secret
              if [[ "$user" != null ]]; then
                chown -v -R "$user":root "${keyDirectory}/pending/per-user/$user"
              else
                chown -v -R root:"$group" "${keyDirectory}/pending/per-group/$group"
              fi
            done < ${keyDirectory}/pending/included-secrets

            # TOOD: Do this atomically
            if [[ -d ${keyDirectory}/active ]]; then
              rm -r ${keyDirectory}/active
            fi
            mv -v ${keyDirectory}/pending ${keyDirectory}/active
          fi
        '';

        deployScriptPhases.secrets = lib.dag.entryBefore ["switch"] ''
          echo "Copying secrets..." >&2

          ssh "$HOST" sudo mkdir -p -m 755 ${keyDirectory}/pending/per-{user,group}
          # TODO: I don't think this works if rsync isn't on the remote's shell.
          # We really just need a single binary we can execute on the remote, like the switch script
          rsync --perms --chmod=750 --chown "root:root" --rsync-path="set -o noglob; sudo rsync" "${includedSecrets}" "$HOST:${keyDirectory}/pending/included-secrets"

          while read -r json; do
            name=$(echo "$json" | jq -r '.name')
            source=$(echo "$json" | jq -r '.source')
            user=$(echo "$json" | jq -r '.user')
            group=$(echo "$json" | jq -r '.group')

            echo "Copying secret $name..." >&2

            # If this is a per-user secret
            if [[ "$user" != null ]]; then
              # The -n is very important for ssh to not swallow stdin!
              ssh -n "$HOST" sudo mkdir -p -m 500 "${keyDirectory}/pending/per-user/$user"
              rsync --perms --chmod=400 --rsync-path="sudo rsync" "$source" "$HOST:${keyDirectory}/pending/per-user/$user/$name"
            else
              ssh -n "$HOST" sudo mkdir -p -m 050 "${keyDirectory}/pending/per-group/$group"
              rsync --perms --chmod=040 --rsync-path="sudo rsync" "$source" "$HOST:${keyDirectory}/pending/per-group/$group/$name"
            fi
          done < "${includedSecrets}"

          echo "Finished copying secrets" >&2
        '';
      };
    });
  };

}
