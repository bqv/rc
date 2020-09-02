nixusArgs: conf: let
  nixpkgs = import ./nixpkgs.nix;

  extendLib = super:
    let
      lib = super.extend (import ./dag.nix);
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
      conf
      # Not naming it pkgs to avoid confusion and trouble for overriding scopes
      {
        _module.args.nixus = {
          pkgs = nixusPkgs;
          inherit extendLib;
        };
      }
    ];
  };
in result.config.deployScript // result // nixusPkgs.lib.mapAttrs (n: v: v.deployScript) result.config.nodes
