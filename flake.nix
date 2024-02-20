{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flakeParts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs@{ self, nixpkgs, flakeParts, ... }:
      flakeParts.lib.mkFlake { inherit inputs; } {
        systems = nixpkgs.lib.systems.flakeExposed;
        imports = [ inputs.haskell-flake.flakeModule ];

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
                haskell-language-server = hsPkgs.haskell-language-server;
              };
              hlsCheck.enable = true;
            };
          };

          packages.default = self'.packages.gltf-loader;
        };
      };
}
