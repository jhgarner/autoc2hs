{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Poke where

import App
import Language.C.Analysis hiding (Type)
import Foreign
import Language.Haskell.TH.Syntax
import TypeLookup
import Data.List (scanl')
import Control.Monad
import Namable

pokeAt :: Name -> Int -> Exp
pokeAt p x =
  AppE (AppE (VarE 'pokeByteOff) (VarE p)) $ LitE $ IntegerL $ fromIntegral x

pokeIt :: CDataType -> Q Dec
pokeIt (Enum _ _) = head <$> [d| poke p a = poke (castPtr p) (fromEnum a) |]
pokeIt (Struct typeName ls) = do
  pName <- newName "p"
  aName <- newName "a"
  binders <- replicateM (length ls) $ newName "x"
  let sizes = fmap (getSize . snd) ls
      conName = toName typeName
      offsets = scanl' (+) 0 sizes
      pokes = fmap (pokeAt pName) offsets
      doBlocks = zipWith AppE pokes $ fmap VarE binders
      body = DoE $ fmap NoBindS doBlocks
      clause = Clause [VarP pName, ConP conName $ fmap VarP binders] (NormalB body) []
      fun = FunD 'poke [clause]
   in pure fun
pokeIt t = error $ "Can't implement poke for this" ++ show t
