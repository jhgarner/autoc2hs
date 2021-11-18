{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Alignment where

import App
import Language.C.Analysis hiding (Type)
import Foreign
import Foreign.C
import Data.Semigroup
import Data.Functor.Foldable

align :: forall a. Storable a => Int
align = alignment @a undefined

alignOfCInt :: IntType -> Int
alignOfCInt t = case t of
  TyBool -> align @CBool
  TyChar -> align @CChar
  TySChar -> align @CSChar
  TyUChar -> align @CUChar
  TyShort -> align @CShort
  TyUShort -> align @CUShort
  TyInt -> align @CInt
  TyUInt -> align @CUInt
  TyLong -> align @CLong
  TyULong -> align @CULong
  TyLLong -> align @CLLong
  TyULLong -> align @CULLong
  TyInt128 -> error "Find a haskell 128 bit int type"
  TyUInt128 -> error "Find a haskell 128 bit int type"

alignOfCFloat :: FloatType -> Int
alignOfCFloat t = case t of
  TyFloat -> align @CFloat
  TyDouble -> align @CDouble
  TyLDouble -> error "Not sure what a long double is"
  TyFloatN _ _ -> error "I don't know what this is"

getalignOf :: CDataTypeF Int -> Int
getalignOf TheVoidF = align @()
getalignOf (LeafIntF i) = alignOfCInt i
getalignOf (LeafFloatF f) = alignOfCFloat f
getalignOf FunctionF = align @()
getalignOf (OpaqueF _) = align @()
getalignOf (PointerF _) = align @(Ptr ())
getalignOf (ArrayF s a) = a
getalignOf (StructF _ ls) = getMax $ foldMap (Max . snd) ls
getalignOf (EnumF _ _) = align @Int

getAlign :: CDataType -> Int
getAlign = cata getalignOf
