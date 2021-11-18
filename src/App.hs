{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module App where

import Prelude hiding (lookup)
import Language.C (SUERef, Ident, Pretty (prettyPrec, pretty), CExpression (CConst), CConstant (CIntConst), CInteger (CInteger))
import Language.C.Analysis
import Language.Haskell.TH.Syntax hiding (lift)
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Reader
import Data.Map.Strict (Map, lookup, insert)
import Control.Monad.Freer.TH
import qualified Language.C.Analysis as C
import Data.Data (TypeRep)
import Data.Functor.Foldable.TH
import Data.Char

data Memo a b r where
  Memo :: a -> Memo a b b

$(makeEffect ''Memo)

type Globals = Reader (Map SUERef TagDef)

runMemo :: forall a b rest r. (Ord a, Member (State (Map a b)) rest) => a -> (a -> Eff (Memo a b ': rest) b) -> Eff rest b
runMemo key eff =
  interpret
    ( \case
        Memo k -> do
          mv <- gets @(Map a b) (lookup k)
          case mv of
            Just v -> pure v
            Nothing -> do
              b <- runMemo k eff
              modify (insert k b)
              pure b
    )
    $ eff key
  

data ATypeName = A SUERef | B Ident
  deriving (Show, Ord, Eq)

instance Pretty ATypeName where
  prettyPrec i (A a) = prettyPrec i a
  prettyPrec i (B b) = prettyPrec i b

data CDataType
  = LeafInt IntType
  | LeafFloat FloatType
  | TheVoid
  | Function
  | Opaque SUERef
  | Pointer (Either SUERef CDataType)
  | Array Int CDataType
  | Struct SUERef [(VarName, CDataType)]
  | Enum SUERef [Ident]
  deriving (Show)

$(makeBaseFunctor ''CDataType)

typeNameLookup :: GlobalWithTree r => TypeName -> Eff r CDataType
typeNameLookup TyVoid = pure TheVoid
typeNameLookup (TyIntegral i) = pure $ LeafInt i
typeNameLookup (TyFloating f) = pure $ LeafFloat f
typeNameLookup (TyComplex f) = pure $ LeafFloat f
typeNameLookup (TyComp (CompTypeRef name _ _)) = depsFromName name
typeNameLookup (TyEnum (EnumTypeRef name _)) = depsFromName name
typeNameLookup (TyBuiltin _) = error "I don't know what this means"

shallowTypeNameLookup :: TypeName -> Either SUERef CDataType
shallowTypeNameLookup TyVoid = Right TheVoid
shallowTypeNameLookup (TyIntegral i) = Right $ LeafInt i
shallowTypeNameLookup (TyFloating f) = Right $ LeafFloat f
shallowTypeNameLookup (TyComplex f) = Right $ LeafFloat f
shallowTypeNameLookup (TyComp (CompTypeRef name _ _)) = Left name
shallowTypeNameLookup (TyEnum (EnumTypeRef name _)) =  Left name
shallowTypeNameLookup (TyBuiltin _) = error "I don't know what this means"

shallowTypeLookup :: C.Type -> Either SUERef CDataType
shallowTypeLookup (PtrType t _ _) = Right $ Pointer $ shallowTypeLookup t
shallowTypeLookup (DirectType name _ _) = shallowTypeNameLookup name
shallowTypeLookup (ArrayType name _ _ _) = shallowTypeLookup name
shallowTypeLookup (FunctionType _ _) = Right Function -- error "Function type detected"
shallowTypeLookup (TypeDefType (TypeDefRef _ t _) _ _) = shallowTypeLookup t

getArraySize :: ArraySize -> Int
getArraySize (UnknownArraySize _) = error "What does this even mean?"
getArraySize (ArraySize _ (CConst (CIntConst (CInteger i _ _) _))) = fromIntegral i
getArraySize _ = error "I really don't want to evaluate c expressions"

typeLookup :: GlobalWithTree r => C.Type -> Eff r CDataType
typeLookup (PtrType t _ _) = pure $ Pointer $ shallowTypeLookup t

typeLookup (DirectType name _ _) = typeNameLookup name
typeLookup (ArrayType name size _ _) = Array (getArraySize size) <$> typeLookup name
typeLookup (FunctionType _ _) = pure Function -- error "Function type detected"
typeLookup (TypeDefType (TypeDefRef _ t _) _ _) = typeLookup t

-- TODO Don't assume that each enum item is assigned in order starting with 0
mkEnum :: EnumType -> CDataType
mkEnum (EnumType name cons _ _) = Enum name $ fmap enumToCon cons 
  where 
    enumToCon (Enumerator name _ _ _) = name

handleMemberDecl :: GlobalWithTree r => MemberDecl -> Eff r (VarName, CDataType)
handleMemberDecl (MemberDecl (VarDecl name _ t) _ _) =
  (name,) <$> typeLookup t
  
handleMemberDecl AnonBitField {} = error "I don't know how to handle anonymous buit fields"

mkStruct :: GlobalWithTree r => CompType -> Eff r CDataType
mkStruct (CompType name _ ls _ _) = do
  xs <- traverse handleMemberDecl ls
  pure $ Struct name xs

directDeps :: GlobalWithTree r => TagDef -> Eff r CDataType
directDeps (CompDef struct) = mkStruct struct
directDeps (EnumDef enum) = pure $ mkEnum enum

-- mkOpaque :: ATypeName -> App [Dec]
-- mkOpaque (A name) = pure [NewtypeD [] (toName name) [] Nothing (NormalC (toName name)
--   [(Bang NoSourceUnpackedness NoSourceStrictness, AppT (ConT ''Ptr) (ConT ''()))]) []]
-- mkOpaque (B name) = error "B"
--
class CToHaskellNamable a where
  toString :: a -> String

instance CToHaskellNamable Ident where
  toString = show . pretty
instance CToHaskellNamable SUERef where
  toString = show . pretty
instance CToHaskellNamable VarName where
  toString = show . pretty

type GlobalWithTree r = Members '[Globals, State (Map SUERef CDataType)] r

depsFromName :: GlobalWithTree r => SUERef -> Eff r CDataType
depsFromName name = do
  exists <- gets $ lookup name
  case exists of
    Just cData -> pure cData
    Nothing -> do
      t <- asks $ lookup name
      c <- case t of
            Just name -> directDeps name
            Nothing -> pure $ Opaque name
      modify (insert name c)
      pure c

-- newtype App a = App {runApp :: StateT (Set ATypeName) (ReaderT GlobalDecls Q) a}
--   deriving (Functor, Applicative, Monad, MonadReader GlobalDecls, MonadState (Set ATypeName), MonadIO, MonadFail, MonadQuasi)

-- class MonadQuasi m where
--   liftQ :: Q a -> m a

-- instance MonadQuasi Q where
--   liftQ = id

-- instance (Monad m, MonadQuasi m) => MonadQuasi (ReaderT r m) where
--   liftQ = lift . liftQ

-- instance (Monad m, MonadQuasi m) => MonadQuasi (StateT r m) where
--   liftQ = lift . liftQ
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
