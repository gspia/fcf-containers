{ 
  # compiler ? "ghc881" 
} :
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        /* overrides = haskellPackagesNew: haskellPackagesOld: rec { */
        overrides = self: super: rec {
          /* aeson = haskellPackagesNew.callPackage ./aeson.nix { }; */
          ghc = super.ghc  // { withPackages = super.ghc.withHoogle; };
          ghcWithPackages = self.ghc.withPackages;
          first-class-families = self.callCabal2nix "first-class-families" 
          #   ~/gito/first-class-families {}; 
            (pkgs.fetchFromGitHub { # 0.6
              owner  = "Lysxia";
              repo   = "first-class-families";
              rev    = "2fb77468d3b0ce64dc5371bed1b636d60219975c";
              sha256 = "1jvn11nbb7271hhy7q21cyn4r238pfzj00aabd39gwhi0g2gzqbf";
            }) {};
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
