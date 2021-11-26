{-# LANGUAGE TemplateHaskell #-}

module Func where
import qualified Language.C.Analysis as C
import Language.Haskell.TH.Syntax
import Language.C.Analysis hiding (Type)
import App
import Namable
import TH
import Foreign.Ptr
import Data.Vector.Fixed.Storable
import Language.C hiding (CCall)
import Data.Map.Strict (Map)
import Data.Functor.Foldable
import Language.C.Data.Ident
import Control.Monad.Free


-- getHaskellType :: CDataTypeF Type -> Type
-- getHaskellType = \case
--   StructF typeName _ -> ConT $ toName typeName
--   EnumF typeName ls -> ConT $ toName typeName
--   OpaqueF typeName -> ConT $ toName typeName
--   LeafIntF i -> cInt2Haskell i
--   LeafFloatF f -> cFloat2Haskell f
--   TheVoidF -> ConT ''()
--   PointerF t -> ConT ''Ptr `AppT` t
--   ArrayF n t -> ConT ''Vec `AppT` LitT (NumTyLit $ fromIntegral n) `AppT` t
--   FunctionF ret params -> foldr (\l r -> ArrowT `AppT` l `AppT` r) (AppT (ConT ''IO) ret) params

startInitial :: Map SUERef TagDef -> Free CDataTypeF SUERef -> CDataType
startInitial m (Pure a) = depsFromName m a
startInitial m (Free c) = embed $ fmap (startInitial m) c



createGlobal :: Map SUERef TagDef -> IdentDecl -> Dec
createGlobal m d@(Declaration (Decl (VarDecl name _ t) _)) = ForeignD $ ImportF CCall Safe (header ++ " " ++ show (pretty name)) (toLowerName name) $ getType $ startInitial m (typeLookup t)
  where header = posFile $ posOf d
createGlobal _ _ = error "NERATHA"
