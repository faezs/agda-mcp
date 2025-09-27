{
  description = "Agda MCP Server - Model Context Protocol server for Agda interaction";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc948.override {
          overrides = final: prev: {
            # Override any Haskell packages if needed
          };
        };

        agda-mcp = haskellPackages.callCabal2nix "agda-mcp" ./. {
          # Agda = haskellPackages.Agda;
        };

      in {
        packages = {
          default = agda-mcp;
          agda-mcp = agda-mcp;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Haskell toolchain
            haskellPackages.ghc
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu

            # Agda
            haskellPackages.Agda

            # Development tools
            pkg-config
            zlib
          ];

          shellHook = ''
            echo "Agda MCP development environment"
            echo "Available commands:"
            echo "  cabal build    - Build the project"
            echo "  cabal run      - Run the MCP server"
            echo "  cabal repl     - Start GHCi"
            echo ""
            echo "Agda version: $(agda --version)"
            echo "GHC version: $(ghc --version)"
          '';
        };

        # For CI/testing
        checks = {
          build = agda-mcp;
        };
      }
    );
}