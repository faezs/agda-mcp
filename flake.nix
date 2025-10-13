{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/e643668fd71b949c53f8626614b21ff71a07379d";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    mcp-server-hs = { url = "github:drshade/haskell-mcp-server"; flake = false; };
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://community.flake.parts/haskell-flake/package-set
          basePackages = pkgs.haskell.packages.ghc910;
          
          # Extra package information. See https://community.flake.parts/haskell-flake/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          packages = {
            mcp-server.source = pkgs.applyPatches {
              name = "mcp-server-patched";
              src = inputs.mcp-server-hs;
              patches = [ ./patches/mcp-server-header-optional.patch ];
            };
            # aeson.source = "1.5.0.0";      # Override aeson to a custom version from Hackage
            # shower.source = inputs.shower; # Override shower to a custom source path
          };
          settings = {
            #  aeson = {
            #    check = false;
            #  };
            #  relude = {
            #    haddock = false;
            #    broken = false;
            #  };
          };

          devShell = {
            # Enabled by default
            # enable = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            tools = hp: {
              hlint = null; # Disable hlint due to GHC 9.10 incompatibility
              haskell-language-server = null; # Disable HLS
            };

            # Check that haskell-language-server works
            # hlsCheck.enable = true; # Requires sandbox to be disabled
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.example;
      };
    };
}
