{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
  ( someFunc,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict (MonadState (put), StateT, get, runStateT)
import Data.Foldable
import Data.Map
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Set
import Debug.Trace (traceShowId, traceShowM)
import Language.C
import Language.C.Analysis
import Language.C.Analysis.DefTable hiding (lookupType)
import Language.C.Analysis.NameSpaceMap (globalNames)
import Language.C.System.GCC (newGCC)
import Prelude hiding (lookup)

someFunc :: IO ()
someFunc = do
  toHCode

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

newtype App a = App {runApp :: StateT (Set ATypeName) (ReaderT GlobalDecls IO) a}
  deriving (Functor, Applicative, Monad, MonadReader GlobalDecls, MonadState (Set ATypeName))

data ATypeName = A SUERef | B Ident
  deriving (Show, Ord, Eq)

instance Pretty ATypeName where
  prettyPrec i (A a) = prettyPrec i a
  prettyPrec i (B b) = prettyPrec i b


lookupType :: ATypeName -> App (Maybe TagDef)
lookupType (A name) = asks $ lookup name . gTags
lookupType (B name) = error "Can't handle B"

typeNameLookup :: TypeName -> App [String]
typeNameLookup TyVoid = pure []
typeNameLookup (TyIntegral i) = pure [show i]
typeNameLookup (TyFloating f) = pure [show f]
typeNameLookup (TyComplex f) = pure [show f]
typeNameLookup (TyComp (CompTypeRef name _ _)) = (show (pretty name) :) <$> depsFromName (A name)
typeNameLookup (TyEnum (EnumTypeRef name _)) = depsFromName $ A name
typeNameLookup (TyBuiltin _) = error "I don't know what this means"

typeLookup :: Type -> App [String]
typeLookup (PtrType t _ _) = typeLookup t
typeLookup (DirectType name _ _) = typeNameLookup name
typeLookup (ArrayType name _ _ _) = typeLookup name
typeLookup (FunctionType _ _) = pure ["A function type"]
typeLookup (TypeDefType (TypeDefRef _ t _) _ _) = typeLookup t

handleMemberDecl :: MemberDecl -> App [String]
handleMemberDecl (MemberDecl (VarDecl _ _ t) _ _) = typeLookup t
handleMemberDecl AnonBitField {} = error "I don't know how to handle anonymous buit fields"

handleEnumerators :: Enumerator -> App [String]
handleEnumerators (Enumerator name _ _ _) = pure [show $ pretty name]

directDeps :: TagDef -> App [String]
directDeps (CompDef (CompType _ _ ls _ _)) = join <$> traverse handleMemberDecl ls
directDeps (EnumDef (EnumType name _ _ _)) = pure [show $ pretty name]

depsFromName :: ATypeName -> App [String]
depsFromName name = do
  cache <- get
  if Data.Set.member name cache
    then pure []
    else do
      put (Data.Set.insert name cache)
      t <- lookupType name
      case t of
        Just name -> directDeps name
        Nothing -> pure []

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

toHCode = do
  Right a <- ast
  let Right (globals, warnings) = runTrav_ $ analyseAST' a
      types = gTags globals
  -- traverse_ (print . show . pretty) types
  -- let Just (CompDef (CompType _ _ ls attrs _)) = lookup (NamedRef (internalIdent "udev")) types
  -- print $ fmap (\(MemberDecl (VarDecl name _ t) _ _) -> "{" ++ show (pretty name) ++ " " ++ show (pretty t) ++ "}") ls
  (x, _) <- runReaderT (runStateT (runApp $ depsFromName $ A $ NamedRef $ internalIdent "wlr_session") mempty) globals
  print x

--     -- defs = gTypeDefs globals
--     -- objs = gObjs globals
-- -- print $ pretty $ snd $ last $ M.toList $ M.filter fromFileFilter types
-- -- print $ snd $ last $ M.toList $ M.filter fromFileFilter types
-- -- print $ lookup (internalIdent "_Bool") objs
-- -- print $ lookup (NamedRef (internalIdent "_Bool")) defs

-- -Iwlroots/include/ -IfakeHeaders/ -IwaylandScanner/ -DWLR_USE_UNSTABLE -I/nix/store/zz6wnl0all718kz9x11fp5331i10mkmw-wayland-1.19.0-dev/include -I/nix/store/j8s8h5h2xyjdcsxv3h8yzwngabfw6sbd-pixman-0.38.4/include/pixman-1 -I/nix/store/fk708siz7r42av7rfy3n0hwv4xqrdm8m-libxkbcommon-1.3.1-dev/include -I/nix/store/x33wa1b417shqy9l2yq2jw2g17z4xk9d-systemd-249.4-dev/include
