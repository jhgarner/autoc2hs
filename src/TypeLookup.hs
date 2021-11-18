{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TypeLookup where

import App
import Language.C.Analysis hiding (Type)
import Foreign
import Foreign.C
import Data.Semigroup
import Data.Functor.Foldable

size :: forall a. Storable a => Int
size = sizeOf @a undefined

sizeOfCInt :: IntType -> Int
sizeOfCInt t = case t of
  TyBool -> size @CBool
  TyChar -> size @CChar
  TySChar -> size @CSChar
  TyUChar -> size @CUChar
  TyShort -> size @CShort
  TyUShort -> size @CUShort
  TyInt -> size @CInt
  TyUInt -> size @CUInt
  TyLong -> size @CLong
  TyULong -> size @CULong
  TyLLong -> size @CLLong
  TyULLong -> size @CULLong
  TyInt128 -> error "Find a haskell 128 bit int type"
  TyUInt128 -> error "Find a haskell 128 bit int type"

sizeOfCFloat :: FloatType -> Int
sizeOfCFloat t = case t of
  TyFloat -> size @CFloat
  TyDouble -> size @CDouble
  TyLDouble -> error "Not sure what a long double is"
  TyFloatN _ _ -> error "I don't know what this is"

getSizeOf :: CDataTypeF Int -> Int
getSizeOf TheVoidF = size @()
getSizeOf (LeafIntF i) = sizeOfCInt i
getSizeOf (LeafFloatF f) = sizeOfCFloat f
getSizeOf FunctionF = size @()
getSizeOf (OpaqueF _) = size @()
getSizeOf (PointerF _) = size @(Ptr ())
getSizeOf (ArrayF n size) = n * size
getSizeOf (StructF _ ls) = getSum $ foldMap (Sum . snd) ls
getSizeOf (EnumF _ _) = size @Int

getSize :: CDataType -> Int
getSize = cata getSizeOf
