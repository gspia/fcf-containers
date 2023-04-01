{
  description = "Data structures and algorithms for first-class-families";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nix-filter.url = "github:numtide/nix-filter/master";
  };

  outputs = { self, nixpkgs, nix-filter }: 
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };

      # filter = import nix-filter { };

      ghcVersion = "925";

      src = nix-filter.lib {
        root = ./.;
        include = [
          (nix-filter.lib.inDirectory "src")
          (nix-filter.lib.inDirectory "examples")
          (nix-filter.lib.inDirectory "test")
          (nix-filter.lib.matchExt "hs")
          ./fcf-containers.cabal
          ./cabal.project
          ./LICENSE
        ];
      };

      fcf-containers = hself: hself.callCabal2nix "fcf-containers" src {};

      myHaskellPackages = pkgs.haskell.packages."ghc${ghcVersion}".override {
        overrides = hself: hsuper: {
          # fcf-containers = hself.callCabal2nix "fcf-containers" sources.fcf-containers {};
          # fcf-containers = (import sources.fcf-containers {});
          fcf-containers = fcf-containers hself;
          # ListLike = pkgs.haskell.lib.dontCheck hsuper.ListLike;
          # type-of-html = pkgs.haskell.lib.doBenchmark (hself.callPackage ./nix/type-of-html.nix {inherit src;});
          # type-of-html = hself.callCabal2nix "type-of-html" src {};
          # optics-core = hsuper.optics-core.overrideAttrs(old: {
          #   configureFlags = "-f explicit-generic-labels";
          #   patches = [./optics-core.patch];
          # });
        };
      };

      shell = myHaskellPackages.shellFor {
        packages = p: [
          p.fcf-containers
        ];
        buildInputs = with pkgs.haskell.packages."ghc${ghcVersion}"; [
          myHaskellPackages.cabal-install
          ghcid
          (pkgs.haskell-language-server.override { supportedGhcVersions = [ "${ghcVersion}" ]; })
          hlint
          # implicit-hie
          # cabal2nix
        ];
        withHoogle = true;
        doBenchmark = true;
      };

    in
      {
        library = fcf-containers;
        # packages.x86_64-linux.default = ;
        devShell.x86_64-linux = shell;
      };
}
