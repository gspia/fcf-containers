{ 
  # compiler ? "ghc884" 
  # compiler ? "ghc8107"
  # compiler ? "ghc902"
  # compiler ? "ghc922"
} :
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
      # haskellPackages = pkgs.haskell.packages.${compiler}.override {
        /* overrides = haskellPackagesNew: haskellPackagesOld: rec { */
        overrides = self: super: rec {
          # ghc = super.ghc;
          # ghc = super.ghc  // { withPackages = super.ghc.withHoogle; };
          # ghcWithPackages = self.ghc.withPackages;
          # mkDerivation = args: super.mkDerivation (args // {
          #   enableLibraryProfiling = true;
          # });
          first-class-families = self.callCabal2nix "first-class-families" 
          #   ~/gito/first-class-families {}; 
            (pkgs.fetchFromGitHub { # 0.8.0.1
              owner  = "Lysxia";
              repo   = "first-class-families";
              rev    = "d4780864ed9b11343b7b26050e39b2048a8e61f6";
              sha256 = "1iw0dryz66z4k9lsj1kayf17xs38i8bdk9aqi2qz49vcbhnfw8pd";
            }) {};
            # something for the next version?
            #  rev    = "9fe4ce36cf1cd4b0f5af59c923c15b9085c48cd6";
            #  sha256 = "1677ylhhf1mwwfs6j2p6gbn2f6mzsx4zmaihz8v9v07h845wz8l7";
            # 0.8.0
            # rev    = "4a0bf3ea9c125bb4009b61ce70b1a5339b7b2072";
            # sha256 = "14387mpfvds226iynkpay3aaqamvxznxjsmg2qcwdxafdvxmyq9z";
            # (pkgs.fetchFromGitHub { # 0.7
            #   owner  = "Lysxia";
            #   repo   = "first-class-families";
            #   rev    = "120e41ea3831b9ffad62239e109c1b2a0aaf269f";
            #   sha256 = "0pyxpnkgr2rgn83whl37jh7k3rdr5hiprdmfkswwj7c1nhl1ky5i";
            # }) {};
        };
      };
    };
    allowUnfree = true;
  };
  nixpkgs = import <nixpkgs> { inherit config; };
  pkgs = nixpkgs.pkgs;
  hpkgs = pkgs.haskellPackages;
  # hpkgs = pkgs.haskell.packages.${compiler};

  adjust-for-ghc = drv: {
    executableSystemDepends = [
      hpkgs.ghcid
      # hpkgs.ghcide
      hpkgs.haskell-language-server
      hpkgs.cabal-install
    ];
    buildDepends = [
      hpkgs.hlint
      # hpkgs.ghc-mod
      # hpkgs.hasktags
      # hpkgs.haskdogs  # stack config set system-ghc --global true 
      # hpkgs.hindent
      # hpkgs.hsimport
      # hpkgs.pointfree
      # hpkgs.pointful
      # hpkgs.stylish-haskell
    ];
  };

  adjust = adjust-for-ghc;

  exe-code-base = hpkgs.callCabal2nix "fcf-containers" ./. {};
  exe-code = pkgs.haskell.lib.overrideCabal exe-code-base adjust;
in
  exe-code
