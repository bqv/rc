nixusArgs: conf: let
  inherit (nixusArgs) nixpkgs;

  extendLib = super:
    let
      lib = super.extend (import ../lib/dag.nix);
      self = if nixusArgs ? libOverlay
             then lib.extend nixusArgs.libOverlay
             else lib;
    in self;

  nixusPkgs = import nixpkgs {
    config = {};
    overlays = [
      (self: super: { lib = extendLib super.lib; })
    ];
    system = nixusArgs.deploySystem or builtins.currentSystem;
  };

  result = nixusPkgs.lib.evalModules {
    modules = [
      modules/options.nix
      modules/deploy.nix
      modules/secrets.nix
      modules/ssh.nix
      modules/public-ip.nix
      modules/dns.nix
      modules/vpn
      conf
      # Not naming it pkgs to avoid confusion and trouble for overriding scopes
      {
        _module.args.nixus = {
          pkgs = nixusPkgs;
          inherit extendLib;
        };
        _module.args.pkgs = throw "You're trying to access the pkgs argument from a Nixus module, use the nixus argument instead and use nixus.pkgs from that.";
      }
    ];
  };
in result
