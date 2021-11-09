let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.pkgconfig
    pkgs.wayland-scanner
    pkgs.wayland-protocols
    pkgs.wayland
    pkgs.libxkbcommon.dev
    pkgs.pixman
    pkgs.libudev
    pkgs.gcc
    (pkgs.haskell-language-server.override { supportedGhcVersions = [ "8107" ]; })
    pkgs.haskellPackages.ormolu
    pkgs.haskellPackages.floskell
    pkgs.haskellPackages.brittany
  ];
}
