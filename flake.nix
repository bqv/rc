{
  description = "Deployment utility";

  outputs = { self, nixpkgs }: {

    lib = {
      nixus = import ./.;
      dag = import ./dag.nix;
    };

  };
}
