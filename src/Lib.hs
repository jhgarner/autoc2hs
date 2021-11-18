module Lib
  ( someFunc, generate
  )
where

import App
import TH
import Control.Monad.IO.Class

import Language.C hiding (CChar, CFloat, Name)
import Language.C.Analysis hiding (Type)
import qualified Language.C.Analysis as C
import Language.C.System.GCC (newGCC)
import Language.Haskell.TH.Syntax hiding (lift)
import AllTypes
import Data.Monoid

someFunc :: IO ()
someFunc = print "Done"

ast = parseCFile (newGCC "gcc") Nothing opts "tmp.h"
-- ast = parseCFile (newGCC "gcc") Nothing opts "./wlroots/include/wlr/backend/session.h"

opts =
  [ "-Iwlroots/include/",
    "-IwaylandScanner/",
    "-DWLR_USE_UNSTABLE",
    "-I/nix/store/zz6wnl0all718kz9x11fp5331i10mkmw-wayland-1.19.0-dev/include",
    "-I/nix/store/j8s8h5h2xyjdcsxv3h8yzwngabfw6sbd-pixman-0.38.4/include/pixman-1",
    "-I/nix/store/fk708siz7r42av7rfy3n0hwv4xqrdm8m-libxkbcommon-1.3.1-dev/include",
    "-I/nix/store/x33wa1b417shqy9l2yq2jw2g17z4xk9d-systemd-249.4-dev/include"
  ]

fromFileFilter :: Pos n => n -> Bool
fromFileFilter n =
  let info = posFile $ posOf n
   in info == "./wlroots/include/wlr/backend/session.h"

generate :: Q [Dec]
generate = do
  Right a <- liftIO ast
  let Right (globals, warnings) = runTrav_ $ analyseAST a
      types = gTags globals
  let c = depsFromName types $ NamedRef $ internalIdent "wlr_session"
      t = getTypes generateType c
  instances <- getAp $ getTypes (Ap . generateSerializable) c
  pure $ t ++ instances
