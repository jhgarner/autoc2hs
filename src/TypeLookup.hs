{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module TypeLookup
  ( typeLookup'
  ) where

import App
import Language.C.Analysis hiding (Type)
import Language.Haskell.TH.Syntax (Type (ConT, AppT), Dec, Name, mkName)
import qualified Language.C.Analysis as C
import Foreign
import Foreign.C
import Language.C hiding (CFloat, CChar, Name)
import Data.Char (toUpper)
import Control.Monad.Freer
import Control.Monad.Freer.Reader

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

sizeOfCInt :: IntType -> Int
sizeOfCInt t = case t of
  TyBool -> sizeOf @CBool undefined
  TyChar -> sizeOf @CChar undefined
  TySChar -> sizeOf @CSChar undefined
  TyUChar -> sizeOf @CUChar undefined
  TyShort -> sizeOf @CShort undefined
  TyUShort -> sizeOf @CUShort undefined
  TyInt -> sizeOf @CInt undefined
  TyUInt -> sizeOf @CUInt undefined
  TyLong -> sizeOf @CLong undefined
  TyULong -> sizeOf @CULong undefined
  TyLLong -> sizeOf @CLLong undefined
  TyULLong -> sizeOf @CULLong undefined
  TyInt128 -> error "Find a haskell 128 bit int type"
  TyUInt128 -> error "Find a haskell 128 bit int type"

sizeOfCFloat :: FloatType -> Int
sizeOfCFloat t = case t of
  TyFloat -> sizeOf @CFloat undefined
  TyDouble -> sizeOf @CDouble undefined
  TyLDouble -> error "Not sure what a long double is"
  TyFloatN _ _ -> error "I don't know what this is"


typeNameLookup :: Member (Memo C.Type Int) r =>  TypeName -> Eff r Int
typeNameLookup TyVoid = pure $ sizeOf @() undefined
typeNameLookup (TyIntegral i) = pure $ sizeOfCInt i
typeNameLookup (TyFloating f) = pure $ sizeOfCFloat f
typeNameLookup (TyComplex f) = pure $ sizeOfCFloat f
typeNameLookup (TyComp (CompTypeRef name _ _)) = memo name
-- TODO Hardcoding CInt probably works most of the time, but I don't think the
-- standard guarantees this.
typeNameLookup (TyEnum (EnumTypeRef name _)) = pure $ sizeOf @CInt undefined
typeNameLookup (TyBuiltin _) = error "I don't know what this means"

typeLookup' :: Member (Memo C.Type Int) r => C.Type -> Eff r Int
typeLookup' (PtrType t _ _) = pure $ sizeOf (undefined :: Ptr ())
typeLookup' (DirectType name _ _) = typeNameLookup name
typeLookup' (ArrayType name _ _ _) = error "Not sure how to handle this"
typeLookup' (FunctionType _ _) = pure $ sizeOf @(Ptr ()) undefined
typeLookup' (TypeDefType (TypeDefRef _ t _) _ _) = memo t

mkStruct :: CompType -> Eff r Int
mkStruct (CompType _ _ ls _ _) = do
  xs <- traverse handleMemberDecl ls
  let rest = xs >>= snd
      thisOne = DataD [] (toName name) [] Nothing [RecC (toName name) $ fmap fst xs] []
  pure $ thisOne : rest

directDeps :: TagDef -> Eff r Int
directDeps (CompDef struct) = mkStruct struct
directDeps (EnumDef enum) = sizeOf @CInt undefined

sizeFromName :: (Member (Memo C.Type Int) r, Member Globals r) => SUERef -> Eff r Int
sizeFromName name = do
  t <- asks $ lookup name
  case t of
    Just name -> directDeps name
    Nothing -> mkOpaque name
