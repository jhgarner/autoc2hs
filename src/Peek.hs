{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Peek where

import App
import Foreign
import Language.Haskell.TH.Syntax
import TypeLookup
import Data.List (scanl', foldl')
import Control.Monad

peekAt :: Name -> Int -> Exp
peekAt p x =
  AppE (AppE (VarE 'peekByteOff) (VarE p)) $ LitE $ IntegerL $ fromIntegral x

peekIt :: CDataType -> Q Dec
peekIt (Enum _ _) = head <$> [d| poke p a = poke (castPtr p) (fromEnum a) |]
peekIt (Struct structName ls) = do
  pName <- newName "p"
  names <- replicateM (length ls) $ newName "x"
  let sizes = fmap (getSize . snd) ls
      offsets = scanl' (+) 0 sizes
      peeks = fmap (peekAt pName) offsets
      patterns = fmap VarP names
      fields = zipWith BindS patterns peeks
      namedVars = fmap VarE names
      constructor = toName structName
      construct = foldl' AppE (ConE constructor) namedVars
      body = DoE $ fields ++ [NoBindS $ AppE (VarE 'pure) construct]
      clause = Clause [VarP pName] (NormalB body) []
      fun = FunD 'peek [clause]
   in pure fun
peekIt t = error $ "Can't implement peek for this" ++ show t

