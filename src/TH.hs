{-# LANGUAGE TemplateHaskell #-}

module TH where

import Language.Haskell.TH.Syntax
import Foreign.Storable
import App
import Language.C.Analysis hiding (Type)
import Foreign.C
import qualified Language.C.Analysis as C
import Foreign.Ptr
import Peek
import Poke
import Data.Traversable
import TypeLookup
import Alignment
import Data.Vector.Fixed.Storable
import Data.Maybe
import Namable

cInt2Haskell :: IntType -> Type
cInt2Haskell = \case
  TyBool -> ConT ''CBool
  TyChar -> ConT ''CChar
  TySChar -> ConT ''CSChar
  TyUChar -> ConT ''CUChar
  TyShort -> ConT ''CShort
  TyUShort -> ConT ''CUShort
  TyInt -> ConT ''CInt
  TyUInt -> ConT ''CUInt
  TyLong -> ConT ''CLong
  TyULong -> ConT ''CULong
  TyLLong -> ConT ''CLLong
  TyULLong -> ConT ''CULLong
  TyInt128 -> error "Find a haskell 128 bit int type"
  TyUInt128 -> error "Find a haskell 128 bit int type"

cFloat2Haskell :: FloatType -> Type
cFloat2Haskell = \case
  TyFloat -> ConT ''CFloat
  TyDouble -> ConT ''CDouble
  TyLDouble -> error "Not sure what a long double is"
  TyFloatN _ _ -> error "I don't know what this is"

getType :: CDataType -> Type
getType = \case
  Struct name _ -> ConT $ toName name
  Enum name _ -> ConT $ toName name
  LeafInt i -> cInt2Haskell i
  LeafFloat f -> cFloat2Haskell f
  TheVoid -> ConT ''()
  Function -> ConT ''()
  Array n t -> ConT ''Vec `AppT` LitT (NumTyLit $ fromIntegral n) `AppT` getType t
  Opaque name -> ConT $ toName name
  Pointer inner -> AppT (ConT ''Ptr) $ getType inner

generateType :: CDataType -> [Dec]
generateType = \case
  Struct typeName ls -> pure $
    let types = fmap (getType . snd) ls
        names = fmap (toLowerName . fst) ls
        bang = repeat $ Bang NoSourceUnpackedness NoSourceStrictness
        fields = zip3 names bang types
        name = toName typeName
        in DataD [] name [] Nothing [RecC name fields] []
  Enum typeName ls -> pure $
    let conNames = fmap (\name -> NormalC (toName name) []) ls
        derive = [DerivClause Nothing [ConT ''Enum]]
     in DataD [] (toName typeName) [] Nothing conNames derive
  Opaque typeName -> pure $
    let name = toName typeName
        bang = Bang NoSourceUnpackedness NoSourceStrictness
        opaqueType = ConT ''()
     in NewtypeD [] name [] Nothing (NormalC name [(bang, opaqueType)]) []
  t -> []


-----------------------------


getName :: CDataType -> Maybe Name
getName = fmap toName . \case
  Struct n _ -> Just n
  Enum n _ -> Just n
  _ -> Nothing

generateSerializable :: CDataType -> Q [Dec]
generateSerializable c = do
  for (maybeToList $ getName c) $ \name -> do
    peekDef <- peekIt c
    pokeDef <- pokeIt c
    let t = AppT (ConT ''Storable) (ConT name)
        size = FunD 'sizeOf [Clause [WildP] (NormalB $ LitE $ IntegerL $ fromIntegral $ getSize c) []]
        align = FunD 'alignment [Clause [WildP] (NormalB $ LitE $ IntegerL $ fromIntegral $ getAlign c) []]
    pure $ InstanceD Nothing [] t [size, align, peekDef, pokeDef]
