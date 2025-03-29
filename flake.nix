{
  description = "Personal flake website";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        # Packages for this system
        pkgs = import nixpkgs { inherit system; };

        # External variables of this package
        websiteVars = rec {
          WEBSITE_SCHEME = "https://";
          WEBSITE_DOMAIN = "www.mista.me";
          WEBSITE_URL = "${WEBSITE_SCHEME}${WEBSITE_DOMAIN}";
        };

        # A shell script that prints the package variables
        # This is needed by the GitHub Actions workflow
        printWebsiteVars = pkgs.writeShellApplication {
          name = "print-website-vars";
          text = ''
            ${builtins.concatStringsSep "\n" (
              builtins.attrValues (builtins.mapAttrs (name: value: "echo ${name}=${value}") websiteVars)
            )}
          '';
        };

        # Override the Haskell package set with our package
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: {
            website = pkgs.haskellPackages.developPackage {
              root = ./.;
              modifier =
                let
                  # Add the website variables to the package environment
                  addWebsiteVars =
                    drv:
                    drv.overrideAttrs (old: {
                      env = (old.env or { }) // websiteVars;
                    });
                  # Add extra build tools to the package environment
                  addBuildTools =
                    drv:
                    pkgs.haskell.lib.addBuildTools drv (
                      with pkgs.haskellPackages;
                      [
                        cabal-install
                        haskell-language-server
                        fourmolu
                      ]
                    );
                in
                drv: addWebsiteVars (addBuildTools drv);
            };
          };
        };
      in
      {
        # Development shell
        devShell = haskellPackages.website;
        # Packages for the website
        packages = rec {
          default = website;
          website = haskellPackages.website;
          print-website-vars = printWebsiteVars;
        };
      }
    );
}
