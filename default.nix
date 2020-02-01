{ 
  # compiler ? "ghc881" 
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
            (pkgs.fetchFromGitHub { # 0.7
              owner  = "Lysxia";
              repo   = "first-class-families";
              rev    = "120e41ea3831b9ffad62239e109c1b2a0aaf269f";
              sha256 = "0pyxpnkgr2rgn83whl37jh7k3rdr5hiprdmfkswwj7c1nhl1ky5i";
            }) {};
            # (pkgs.fetchFromGitHub { # 0.6 TODO remove this
            #   owner  = "Lysxia";
            #   repo   = "first-class-families";
            #   rev    = "2fb77468d3b0ce64dc5371bed1b636d60219975c";
            #   sha256 = "1jvn11nbb7271hhy7q21cyn4r238pfzj00aabd39gwhi0g2gzqbf";
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
