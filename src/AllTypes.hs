{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module AllTypes where

import App
import Language.C.Analysis hiding (Type)
import Foreign
import Foreign.C
import Data.Semigroup
import Data.Functor.Foldable
import Data.Set hiding (fold)
import Language.C (SUERef)

getTypesOf :: CDataTypeF (Set SUERef) -> Set SUERef
getTypesOf TheVoidF = mempty
getTypesOf (LeafIntF i) = mempty
getTypesOf (LeafFloatF f) = mempty
getTypesOf FunctionF = mempty
getTypesOf (OpaqueF _) = mempty
getTypesOf (PointerF (Left name)) = insert name mempty
getTypesOf (PointerF (Right names)) = names
getTypesOf (ArrayF _ n) = n
getTypesOf (StructF name ls) = insert name $ foldMap snd ls
getTypesOf (EnumF name _) = insert name mempty

getTypes :: CDataType -> Set SUERef
getTypes = cata getTypesOf

