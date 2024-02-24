{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flakeParts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, flakeParts, ... }:
    flakeParts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
      ];

      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          projectFlakeName = "gltf-loader";

          settings = {
            gltf-codec = {
              broken = false;
              check = false;
            };
          };

          devShell = {
            enable = true;

            tools = hsPkgs: {
              cabal-install = hsPkgs.cabal-install;
              fourmolu = hsPkgs.fourmolu;
              haskell-language-server = hsPkgs.haskell-language-server;
              nixpkgs-fmt = pkgs.nixpkgs-fmt;
              hlint = hsPkgs.hlint;
            };
          };
        };

        treefmt.config = {
          projectRootFile = "flake.nix";
          programs = {
            nixpkgs-fmt.enable = true;
            fourmolu.enable = true;
            hlint.enable = true;
          };
        };

        packages.default = self'.packages.gltf-loader;
      };
    };
}
