{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module AllTypes where

import App
import Language.C.Analysis hiding (Type)
import Data.Functor.Foldable hiding (fold)
import Data.Set hiding (fold)
import Language.C (SUERef)
import Control.Monad.State.Strict
import Control.Monad.Identity
import Data.Foldable

type Visited a = StateT (Set SUERef) Identity a

getTypesOf :: Monoid a => (CDataType -> a) -> CDataTypeF (CDataType, Visited a) -> Visited a
getTypesOf f = \case
  TheVoidF -> pure mempty
  LeafIntF _ -> pure mempty
  LeafFloatF _ -> pure mempty
  FunctionF -> pure mempty
  PointerF t -> snd t
  ArrayF _ n -> snd n
  OpaqueF name -> do
    gets (member name) >>= \case
      False -> do
        modify' $ insert name
        pure $ f $ Opaque name
      True -> pure mempty
  c@(StructF name ls) -> do
    gets (member name) >>= \case
      False -> do
        modify' $ insert name
        as <- traverse (snd . snd) ls
        let result = fold as <> f (embed $ fmap fst c)
        pure result
      True -> pure mempty
  c@(EnumF name _) -> do
    gets (member name) >>= \case
      False -> do
        modify' $ insert name
        pure $ f $ embed $ fmap fst c
      True -> pure mempty

getTypes :: Monoid a => (CDataType -> a) -> CDataType -> a
getTypes f c = evalState (para (getTypesOf f) c) mempty

