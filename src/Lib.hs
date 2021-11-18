{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lib
  ( someFunc, generate
  )
where

import App
import TypeLookup
import TH
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.Foldable hiding (toList)
import Data.Map hiding (mapMaybe)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, catMaybes, mapMaybe)
import Debug.Trace (traceShowId, traceShowM)
import Language.C hiding (CChar, CFloat, Name)
import Language.C.Analysis hiding (Type)
import qualified Language.C.Analysis as C
import Language.C.Analysis.DefTable hiding (lookupType)
import Language.C.Analysis.NameSpaceMap (globalNames)
import Language.C.System.GCC (newGCC)
import Prelude hiding (lookup)
import Language.Haskell.TH.Syntax hiding (lift)
import Data.Char (toUpper)
import Foreign.Ptr
import Foreign.C
import Foreign.Storable (sizeOf)
import Control.Monad.Freer.Reader
import Control.Monad.Freer
import Control.Monad.Freer.State
import AllTypes


someFunc :: IO ()
someFunc = do
  print "Done"

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




-- listDeps :: Map SUERef TagDef -> SUERef -> [String]
-- listDeps m at =
--   let x = m M.! at
--       deps =

-- | get the globally defined entries of a definition table
globalDefs' :: DefTable -> GlobalDecls
globalDefs' deftbl = M.foldrWithKey insertDecl (GlobalDecls e gtags e) (globalNames $ identDecls deftbl)
  where
    e = M.empty
    (_fwd_decls, gtags) = M.mapEither id $ globalNames (tagDecls deftbl)
    insertDecl ident (Left tydef) ds = ds {gTypeDefs = M.insert ident tydef (gTypeDefs ds)}
    insertDecl ident (Right obj) ds = ds {gObjs = M.insert ident obj (gObjs ds)}

analyseAST' :: (MonadTrav m) => CTranslUnit -> m GlobalDecls
analyseAST' (CTranslUnit decls _file_node) = do
  -- analyse all declarations, but recover from errors
  mapRecoverM_ analyseExt decls
  -- check we are in global scope afterwards
  getDefTable >>= \dt ->
    unless (inFileScope dt) $
      error "Internal Error: Not in filescope after analysis"
  -- get the global definition table (XXX: remove ?)
  -- getDefTable >>= traceShowM . M.keys . globalNames . identDecls
  fmap globalDefs' getDefTable
  where
    mapRecoverM_ f = mapM_ (handleTravError . f)

-- toHCode = do
--   Right a <- ast
--   let Right (globals, warnings) = runTrav_ $ analyseAST' a
--       types = gTags globals
--   -- traverse_ (print . show . pretty) types
--   -- let Just (CompDef (CompType _ _ ls attrs _)) = lookup (NamedRef (internalIdent "udev")) types
--   -- print $ fmap (\(MemberDecl (VarDecl name _ t) _ _) -> "{" ++ show (pretty name) ++ " " ++ show (pretty t) ++ "}") ls
--   -- (x, _) <- runQ $ runReaderT (runStateT (runApp $ depsFromName $ A $ NamedRef $ internalIdent "wlr_session") mempty) globals
--   print x

generate :: Q [Dec]
generate = do
  Right a <- liftIO ast
  let Right (globals, warnings) = runTrav_ $ analyseAST' a
      types = gTags globals
  -- traverse_ (print . show . pretty) types
  -- let Just (CompDef (CompType _ _ ls attrs _)) = lookup (NamedRef (internalIdent "udev")) types
  -- print $ fmap (\(MemberDecl (VarDecl name _ t) _ _) -> "{" ++ show (pretty name) ++ " " ++ show (pretty t) ++ "}") ls
  -- (x, _) <- runQ $ runReaderT (runStateT (runApp $ depsFromName $ A $ NamedRef $ internalIdent "wlr_session") mempty) globals
  let c = run $ evalState (mempty @(Map SUERef CDataType)) $ runReader types (depsFromName $ NamedRef $ internalIdent "wlr_session")
      cs = foldMap (run . execState (mempty @(Map SUERef CDataType)) . runReader types . depsFromName) $ getTypes c
      t = mapMaybe generateType $ elems cs
  liftIO $ print $ keys cs
  instances <- catMaybes <$> traverse generateSerializable (elems cs)
  pure $ t ++ instances

--     -- defs = gTypeDefs globals
--     -- objs = gObjs globals
-- -- print $ pretty $ snd $ last $ M.toList $ M.filter fromFileFilter types
-- -- print $ snd $ last $ M.toList $ M.filter fromFileFilter types
-- -- print $ lookup (internalIdent "_Bool") objs
-- -- print $ lookup (NamedRef (internalIdent "_Bool")) defs

-- -Iwlroots/include/ -IfakeHeaders/ -IwaylandScanner/ -DWLR_USE_UNSTABLE -I/nix/store/zz6wnl0all718kz9x11fp5331i10mkmw-wayland-1.19.0-dev/include -I/nix/store/j8s8h5h2xyjdcsxv3h8yzwngabfw6sbd-pixman-0.38.4/include/pixman-1 -I/nix/store/fk708siz7r42av7rfy3n0hwv4xqrdm8m-libxkbcommon-1.3.1-dev/include -I/nix/store/x33wa1b417shqy9l2yq2jw2g17z4xk9d-systemd-249.4-dev/include
