let
  pkgs = import <nixpkgs> {};
  # meson = pkgs.meson.overrideAttrs (old: rec {
  #   version = "0.60.2";
  #   src = pkgs.python3.pkgs.fetchPypi {
  #     inherit version;
  #     pname = old.pname;
  #     sha256 = "64e6968565bf1b8152f4f9d6ca8154efb9e14caa9aabf7b22e71e6c5d053e921";
  #   };
  # });
in with pkgs;
pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.pkgconfig
    pkgs.libGL
    pkgs.xwayland
    pkgs.meson
    pkgs.seatd
    pkgs.ninja
    pkgs.pkgconfig
    pkgs.mesa
    pkgs.wayland-scanner
    pkgs.wayland-protocols
    pkgs.libinput
    pkgs.wayland
    pkgs.libdrm
    pkgs.cmake
    pkgs.libxkbcommon.dev
    pkgs.libxkbcommon
    pkgs.pixman
    pkgs.libudev
    pkgs.gcc
    (pkgs.haskell-language-server.override { supportedGhcVersions = [ "8107" ]; })
    pkgs.haskellPackages.ormolu
    pkgs.haskellPackages.floskell
    pkgs.haskellPackages.brittany
  ] ++ [meson ninja pkg-config wayland-scanner
 libGL wayland wayland-protocols libinput libxkbcommon pixman
 xorg.xcbutilwm xorg.libX11 libcap xorg.xcbutilimage xorg.xcbutilerrors mesa
 libpng ffmpeg xorg.xcbutilrenderutil seatd];
}
