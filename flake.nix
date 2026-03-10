{
  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz";
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
    opam-nix.url = "github:tweag/opam-nix";
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
  };
  outputs =
    {
      systems,
      flake-parts,
      opam-nix,
      opam-repository,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        flake-parts.flakeModules.easyOverlay
      ];

      systems = import systems;

      perSystem =
        { pkgs, system, ... }:
        let
          on = opam-nix.lib.${system};

          basePackagesQuery = {
            ocaml-base-compiler = "*";
            map = "*";
          };
          devPackagesQuery = {
            ocamlformat = "*";
            ocaml-lsp-server = "*";
          };

          scope = on.buildOpamProject' { repos = [ opam-repository ]; } ./. (
            basePackagesQuery // devPackagesQuery
          );

          devPackages = builtins.attrValues (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope);
        in
        {
          packages = rec {
            inherit (scope) map;
            default = map;
          };

          overlayAttrs = {
            inherit (scope) map;
          };

          devShells.default = pkgs.mkShell {
            name = "map-dev";
            inputsFrom = [ scope.map ];
            nativeBuildInputs = devPackages;
          };
        };
    };
}
