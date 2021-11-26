{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Lib
import Foreign.C

main :: IO ()
main = someFunc

$(generate 
  [ "-Iwlroots/include/",
    "-IwaylandScanner/",
    "-DWLR_USE_UNSTABLE",
    "-I/nix/store/zz6wnl0all718kz9x11fp5331i10mkmw-wayland-1.19.0-dev/include",
    "-I/nix/store/j8s8h5h2xyjdcsxv3h8yzwngabfw6sbd-pixman-0.38.4/include/pixman-1",
    "-I/nix/store/fk708siz7r42av7rfy3n0hwv4xqrdm8m-libxkbcommon-1.3.1-dev/include",
    "-I/nix/store/x33wa1b417shqy9l2yq2jw2g17z4xk9d-systemd-249.4-dev/include"
  ]
  "wlroots/include/wlr/backend/session.h")
