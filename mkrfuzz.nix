let pkgs = import <nixpkgs> {};
in rec {
  mkrfuzz = pkgs.haskellPackages.callPackage ./default.nix {};
  test = pkgs.runCommand "mkrfuzz" {} ''
    ${mkrfuzz}/bin/mkrfuzz
  '';
}
