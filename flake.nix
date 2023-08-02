{
  description = "A Nix flake for a blog-post generating script, written in Haskell, with a seal bent.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
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
        packages.default = pkgs.runCommand "generateSealPosts" { } ''
          echo Generating seal posts
          mkdir -p $out/bin
          ${ghc'}/bin/ghc                 \
            -O2                           \
            -static                       \
            -o $out/bin/generateSealPosts \
            ${./generateSealPosts.hs}
        '';

        devShells.default = pkgs.mkShell {
          name = "seal-generator-shell";
          buildInputs = with pkgs.haskellPackages;
            [
              ghc'
              hlint
              haskell-language-server
            ];
        };
      });
}
