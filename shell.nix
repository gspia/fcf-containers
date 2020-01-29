{ nixpkgs ? import <nixpkgs> {}
/* , compiler ? "ghc822" */
} : 
let
  inherit (nixpkgs) pkgs;
  /* drv = import ./. { inherit compiler; }; */
  drv = import ./. { };
in
if pkgs.lib.inNixShell then drv.env else drv
