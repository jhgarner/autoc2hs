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
import Data.Map.Strict (keys, (!))
import Func
import qualified Data.Map.Strict as M
import Debug.Trace

someFunc :: IO ()
someFunc = print "Done"

-- ast = parseCFile (newGCC "gcc") Nothing opts "tmp.h"
-- ast = parseCFile (newGCC "gcc") Nothing opts "./wlroots/include/wlr/backend/session.h"


fromFileFilter :: (Pos n, Show n) => String -> n -> Bool
fromFileFilter name n =
  let info = posFile $ posOf n
      isReal = isSourcePos $ posOf n
   in isReal && info == name

extractIdent :: SUERef -> Maybe Ident
extractIdent (NamedRef i) = Just i
extractIdent _ = Nothing

generate :: [String] -> String -> Q [Dec]
generate opts header = do
  Right a <- liftIO $ parseCFile (newGCC "gcc") Nothing opts header
  let Right (globals, warnings) = runTrav_ $ analyseAST a
      types = gTags globals
      funcs = gObjs globals
      roots = filter (maybe False (fromFileFilter header) . extractIdent) $ keys types
      imports = fmap (createGlobal types) $ M.elems $ M.filterWithKey (const . fromFileFilter header) funcs
  liftIO $ print $ funcs ! internalIdent "wlr_session_change_vt"
  let c = depsFromName types <$> roots
      t = getTypes generateType c
  instances <- getAp $ getTypes (Ap . generateSerializable) c
  pure $ t ++ instances ++ imports
