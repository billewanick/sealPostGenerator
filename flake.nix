{
  description = "A Nix flake for a blog-post generating script, written in Haskell, with a seal bent.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        ghc' = pkgs.haskellPackages.ghcWithHoogle (self: with self; [
          dhall
          neat-interpolation
          random
        ]);
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs.haskellPackages;
            [
              ghc'
              hlint
              haskell-language-server
            ];
        };
      });
}
