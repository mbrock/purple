let dapphub = import (
  (import <nixpkgs> {}).fetchFromGitHub {
    owner = "dapphub";
    repo = "nixpkgs-dapphub";
    rev = "3d1d174bbed513f942aa049e6091909fdd3271ec";
    sha256 = "0bhm8dza5c1yc54kabxrfxyam7icqs3i0p35znzhmm6m7grmimkd";
    fetchSubmodules = true;
  }
) {};

in rec {
  mkrfuzz = dapphub.haskellPackages.callPackage ./default.nix { };
  test = dapphub.runCommand "mkrfuzz" {} ''
    ${mkrfuzz}/bin/mkrfuzz
  '';
}
