{
  description = "ormolu Haskell formatter";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: builtins.listToAttrs (map (system: { name = system; value = f system; }) systems);
    in
    {
      packages = forAllSystems (system: {
        default = nixpkgs.legacyPackages.${system}.ormolu;
      });
    };
}
