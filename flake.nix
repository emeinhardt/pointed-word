{
  description = "A Haskell package for pointed words.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Change 945 appropriately to select a different GHC version
        # Check `nix-env -f "<nixpkgs>" -qaP -A haskell.compiler` to see available versions.
        haskellPackages = pkgs.haskell.packages.ghc945;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "pointed-word";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
          };

        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [

            cabal-install

            haskellPackages.haskell-language-server

            haskellPackages.hlint
            haskellPackages.stylish-haskell
            haskellPackages.hindent

            # Optional: Only needed to make use of ./justfile
            just

            # Optional: Only needed to make use of ./dev/cabal-gen-docs.sh
            coreutils
          ];
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;
      });
}
