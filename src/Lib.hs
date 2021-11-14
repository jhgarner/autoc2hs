{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

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
import Control.Monad.Reader
import Control.Monad.State.Strict (MonadState (put), StateT, get, runStateT)
import Data.Foldable
import Data.Map
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Set
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



lookupType :: ATypeName -> App (Maybe TagDef)
lookupType (A name) = asks $ lookup name . gTags
lookupType (B name) = error "Can't handle B"

cInt2Haskell :: IntType -> App Type
cInt2Haskell t = liftQ $ case t of
  TyBool -> [t| CBool |]
  TyChar -> [t| CChar |]
  TySChar -> [t| CSChar |]
  TyUChar -> [t| CUChar |]
  TyShort -> [t| CShort |]
  TyUShort -> [t| CUShort |]
  TyInt -> [t| CInt |]
  TyUInt -> [t| CUInt |]
  TyLong -> [t| CLong |]
  TyULong -> [t| CULong |]
  TyLLong -> [t| CLLong |]
  TyULLong -> [t| CULLong |]
  TyInt128 -> error "Find a haskell 128 bit int type"
  TyUInt128 -> error "Find a haskell 128 bit int type"

cFloat2Haskell :: FloatType -> App Type
cFloat2Haskell t = liftQ $ case t of
  TyFloat -> [t| CFloat |]
  TyDouble -> [t| CDouble |]
  TyLDouble -> error "Not sure what a long double is"
  TyFloatN _ _ -> error "I don't know what this is"

typeNameLookup :: TypeName -> App (Type, [Dec])
typeNameLookup TyVoid = pure (ConT ''(), [])
typeNameLookup (TyIntegral i) = (, []) <$> cInt2Haskell i
typeNameLookup (TyFloating f) = (, []) <$> cFloat2Haskell f
typeNameLookup (TyComplex f) = (, []) <$> cFloat2Haskell f
typeNameLookup (TyComp (CompTypeRef name _ _)) = (ConT $ toName name,) <$> depsFromName (A name)
typeNameLookup (TyEnum (EnumTypeRef name _)) = (ConT $ toName name,) <$> depsFromName (A name)
typeNameLookup (TyBuiltin _) = error "I don't know what this means"

typeLookup :: C.Type -> App (Type, [Dec])
typeLookup (PtrType t _ _) = do
  (innerType, rest) <- typeLookup t
  theType <- liftQ [t| Ptr $(pure innerType) |]
  pure (theType, rest)

typeLookup (DirectType name _ _) = typeNameLookup name
typeLookup (ArrayType name _ _ _) = typeLookup name
typeLookup (FunctionType _ _) = pure (ConT ''(), []) -- error "Function type detected"
typeLookup (TypeDefType (TypeDefRef _ t _) _ _) = typeLookup t

handleMemberDecl :: MemberDecl -> App (VarBangType, [Dec])
handleMemberDecl (MemberDecl (VarDecl name _ t) _ _) = do
  (theType, rest) <- typeLookup t
  pure ((toLowerName name, Bang NoSourceUnpackedness NoSourceStrictness, theType), rest)
handleMemberDecl AnonBitField {} = error "I don't know how to handle anonymous buit fields"

class CToHaskellNamable a where
  toString :: a -> String

toName :: CToHaskellNamable a => a -> Name
toName = mkName . capFirst . mkNice . toString

toLowerName :: CToHaskellNamable a => a -> Name
toLowerName = mkName . mkNice . toString

mkNice :: String -> String
mkNice ('$':xs) = "anon" ++ xs
mkNice name = name

capFirst :: String -> String
capFirst [] = error "Invalid name"
capFirst (x:xs) = toUpper x : xs

instance CToHaskellNamable Ident where
  toString = show . pretty
instance CToHaskellNamable SUERef where
  toString = show . pretty
instance CToHaskellNamable VarName where
  toString = show . pretty

-- TODO Don't assume that each enum item is assigned in order starting with 0
mkEnum :: EnumType -> App Dec
mkEnum (EnumType name cons _ _) = pure $
  DataD [] typeName [] Nothing (fmap enumToCon cons) [DerivClause Nothing [ConT ''Enum]] 
  where 
    typeName = toName name
    enumToCon (Enumerator name _ _ _) = NormalC (toName name) []


member2Size :: MemberDecl -> App Int
member2Size (MemberDecl (VarDecl name _ t) _ _) = do
  (theType, _) <- typeLookup t
  $(thSizeOf $ typeLookup' t)

totalSizeOf :: [MemberDecl] -> App Int
totalSizeOf ls = do
  undefined

mkStruct :: CompType -> App [Dec]
mkStruct (CompType name _ ls _ _) = do
  xs <- traverse handleMemberDecl ls
  let rest = xs >>= snd
      thisOne = DataD [] (toName name) [] Nothing [RecC (toName name) $ fmap fst xs] []
  pure $ thisOne : rest

directDeps :: TagDef -> App [Dec]
directDeps (CompDef struct) = mkStruct struct
directDeps (EnumDef enum) = pure <$> mkEnum enum

mkOpaque :: ATypeName -> App [Dec]
mkOpaque (A name) = pure [NewtypeD [] (toName name) [] Nothing (NormalC (toName name)
  [(Bang NoSourceUnpackedness NoSourceStrictness, AppT (ConT ''Ptr) (ConT ''()))]) []]
mkOpaque (B name) = error "B"

depsFromName :: ATypeName -> App [Dec]
depsFromName name = do
  cache <- get
  if Data.Set.member name cache
    then pure []
    else do
      put (Data.Set.insert name cache)
      t <- lookupType name
      case t of
        Just name -> directDeps name
        Nothing -> mkOpaque name

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
  (x, _) <- runQ $ runReaderT (runStateT (runApp $ depsFromName $ A $ NamedRef $ internalIdent "wlr_session") mempty) globals
  print x

generate :: Q [Dec]
generate = do
  Right a <- liftIO ast
  let Right (globals, warnings) = runTrav_ $ analyseAST' a
      types = gTags globals
  -- traverse_ (print . show . pretty) types
  -- let Just (CompDef (CompType _ _ ls attrs _)) = lookup (NamedRef (internalIdent "udev")) types
  -- print $ fmap (\(MemberDecl (VarDecl name _ t) _ _) -> "{" ++ show (pretty name) ++ " " ++ show (pretty t) ++ "}") ls
  (x, _) <- runQ $ runReaderT (runStateT (runApp $ depsFromName $ A $ NamedRef $ internalIdent "wlr_session") mempty) globals
  pure x


--     -- defs = gTypeDefs globals
--     -- objs = gObjs globals
-- -- print $ pretty $ snd $ last $ M.toList $ M.filter fromFileFilter types
-- -- print $ snd $ last $ M.toList $ M.filter fromFileFilter types
-- -- print $ lookup (internalIdent "_Bool") objs
-- -- print $ lookup (NamedRef (internalIdent "_Bool")) defs

-- -Iwlroots/include/ -IfakeHeaders/ -IwaylandScanner/ -DWLR_USE_UNSTABLE -I/nix/store/zz6wnl0all718kz9x11fp5331i10mkmw-wayland-1.19.0-dev/include -I/nix/store/j8s8h5h2xyjdcsxv3h8yzwngabfw6sbd-pixman-0.38.4/include/pixman-1 -I/nix/store/fk708siz7r42av7rfy3n0hwv4xqrdm8m-libxkbcommon-1.3.1-dev/include -I/nix/store/x33wa1b417shqy9l2yq2jw2g17z4xk9d-systemd-249.4-dev/include
