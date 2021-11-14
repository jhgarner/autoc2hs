{-# LANGUAGE TemplateHaskell #-}

module TH where
import Language.Haskell.TH.Syntax
import Foreign.Storable

thSizeOf theType = AppE (VarE 'sizeOf) (SigE (VarE 'undefined) theType)

-- cSizeOf :: IntType -> App Type
-- cSizeOf t = liftQ $ case t of
--   TyBool -> [t| CBool |]
--   TyChar -> [t| CChar |]
--   TySChar -> [t| CSChar |]
--   TyUChar -> [t| CUChar |]
--   TyShort -> [t| CShort |]
--   TyUShort -> [t| CUShort |]
--   TyInt -> [t| CInt |]
--   TyUInt -> [t| CUInt |]
--   TyLong -> [t| CLong |]
--   TyULong -> [t| CULong |]
--   TyLLong -> [t| CLLong |]
--   TyULLong -> [t| CULLong |]
--   TyInt128 -> error "Find a haskell 128 bit int type"
--   TyUInt128 -> error "Find a haskell 128 bit int type"

-- import Language.Haskell.TH
-- import Control.Monad.IO.Class

-- generate :: Q [Dec]
-- generate = do
--   Right a <- liftIO ast
--   let Right (globals, warnings) = runTrav_ $ analyseAST' a
--       types = gTags globals
--   -- traverse_ (print . show . pretty) types
--   -- let Just (CompDef (CompType _ _ ls attrs _)) = lookup (NamedRef (internalIdent "udev")) types
--   -- print $ fmap (\(MemberDecl (VarDecl name _ t) _ _) -> "{" ++ show (pretty name) ++ " " ++ show (pretty t) ++ "}") ls
--   (x, _) <- runQ $ runReaderT (runStateT (runApp $ depsFromName $ A $ NamedRef $ internalIdent "wlr_session") mempty) globals
--   pure x
