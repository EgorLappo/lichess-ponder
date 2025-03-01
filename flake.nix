{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    nix-github-actions = {
      url = "github:nix-community/nix-github-actions";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, nix-github-actions, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      debug = true;

      flake = {
        githubActions = nix-github-actions.lib.mkGithubMatrix {
          checks =
            nixpkgs.lib.getAttrs [ "x86_64-linux" ] self.checks;
        };
      };

      perSystem = { self', pkgs, ... }: {

        haskellProjects.default = {
          # basePackages = pkgs.haskellPackages;

          packages = {
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

            tools = hp: {
              ormolu = hp.ormolu;
              hpack = hp.hpack;
            };

            # Check that haskell-language-server works
            # hlsCheck.enable = true; # Requires sandbox to be disabled
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.lichess-ponder;
        checks.lichess-ponder = self'.packages.lichess-ponder;
      };
    };
}
