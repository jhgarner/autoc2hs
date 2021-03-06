{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module App where

import Prelude hiding (lookup)
import Language.C (SUERef, Ident, Pretty (prettyPrec, pretty), CExpression (CConst), CConstant (CIntConst), CInteger (CInteger))
import Language.C.Analysis
import Language.Haskell.TH.Syntax hiding (lift)
import Data.Map.Strict (Map, lookup, insert)
import qualified Language.C.Analysis as C
import Data.Functor.Foldable.TH
import Control.Monad.Free
import Data.Functor.Foldable

data CDataType
  = LeafInt IntType
  | LeafFloat FloatType
  | TheVoid
  | Function CDataType [CDataType]
  | Opaque SUERef
  | Pointer CDataType
  | Array Int CDataType
  | Struct SUERef [(VarName, CDataType)]
  | Enum SUERef [Ident]
  deriving (Show)

$(makeBaseFunctor ''CDataType)

typeNameLookup :: TypeName -> Free CDataTypeF SUERef
typeNameLookup TyVoid = Free TheVoidF
typeNameLookup (TyIntegral i) = Free $ LeafIntF i
typeNameLookup (TyFloating f) = Free $ LeafFloatF f
typeNameLookup (TyComplex f) = Free $ LeafFloatF f
typeNameLookup (TyComp (CompTypeRef name _ _)) = Pure name
typeNameLookup (TyEnum (EnumTypeRef name _)) = Pure name
typeNameLookup (TyBuiltin _) = error "I don't know what this means"

getArraySize :: ArraySize -> Int
getArraySize (UnknownArraySize _) = error "What does this even mean?"
getArraySize (ArraySize _ (CConst (CIntConst (CInteger i _ _) _))) = fromIntegral i
getArraySize _ = error "I really don't want to evaluate c expressions"

mkFunctionType :: FunType -> Free CDataTypeF SUERef
mkFunctionType (FunType ret params _) = Free $ FunctionF (typeLookup ret) $ fmap typeOfDecl params
mkFunctionType (FunTypeIncomplete _) = error "Don't know how to handle this"

typeOfDecl :: ParamDecl -> Free CDataTypeF SUERef
typeOfDecl (getVarDecl -> VarDecl _ _ t) = typeLookup t

typeLookup :: C.Type -> Free CDataTypeF SUERef
typeLookup (PtrType t _ _) = Free $ PointerF $ typeLookup t
typeLookup (DirectType name _ _) = typeNameLookup name
typeLookup (ArrayType name size _ _) = Free $ ArrayF (getArraySize size) $ typeLookup name
typeLookup (FunctionType f _) = mkFunctionType f
typeLookup (TypeDefType (TypeDefRef _ t _) _ _) = typeLookup t

-- TODO Don't assume that each enum item is assigned in order starting with 0
mkEnum :: EnumType -> CDataTypeF a
mkEnum (EnumType name cons _ _) = EnumF name $ fmap enumToCon cons 
  where 
    enumToCon (Enumerator name _ _ _) = name

handleMemberDecl :: MemberDecl -> (VarName, Free CDataTypeF SUERef)
handleMemberDecl (MemberDecl (VarDecl name _ t) _ _) = (name, typeLookup t)
handleMemberDecl AnonBitField {} = error "I don't know how to handle anonymous buit fields"

mkStruct :: CompType -> CDataTypeF (Free CDataTypeF SUERef)
mkStruct (CompType name _ ls _ _) = StructF name $ fmap handleMemberDecl ls

directDeps :: TagDef -> CDataTypeF (Free CDataTypeF SUERef)
directDeps (CompDef struct) = mkStruct struct
directDeps (EnumDef enum) = mkEnum enum

depsFromName :: Map SUERef TagDef -> SUERef -> CDataType
depsFromName m = futu \name -> maybe (OpaqueF name) directDeps $ lookup name m
