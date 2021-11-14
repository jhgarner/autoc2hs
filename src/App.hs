{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module App where

import Prelude hiding (lookup)
import Data.Set (Set)
import Language.C (SUERef, Ident, Pretty (prettyPrec))
import Language.C.Analysis (GlobalDecls, TagDef, IntType, FloatType)
import Language.Haskell.TH.Syntax hiding (lift)
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Reader
import Data.Map.Strict (Map, lookup, insert)
import Control.Monad.Freer.TH

data Memo a b r where
  Memo :: a -> Memo a b b

$(makeEffect ''Memo)

type Globals = Reader (Map SUERef TagDef)

runMemo :: forall a b rest r. (Ord a, Member (State (Map a b)) rest) => a -> (a -> Eff (Memo a b ': rest) b) -> Eff rest b
runMemo key eff =
  interpret
    ( \case
        Memo k -> do
          mv <- gets @(Map a b) (lookup k)
          case mv of
            Just v -> pure v
            Nothing -> do
              b <- runMemo k eff
              modify (insert k b)
              pure b
    )
    $ eff key
  

data ATypeName = A SUERef | B Ident
  deriving (Show, Ord, Eq)

instance Pretty ATypeName where
  prettyPrec i (A a) = prettyPrec i a
  prettyPrec i (B b) = prettyPrec i b

data CDataType
  = LeafInt IntType
  | LeafFloat FloatType
  | Pointer CDataType
  | Struct String [(String, CDataType)]
  | Enum String [String]



-- newtype App a = App {runApp :: StateT (Set ATypeName) (ReaderT GlobalDecls Q) a}
--   deriving (Functor, Applicative, Monad, MonadReader GlobalDecls, MonadState (Set ATypeName), MonadIO, MonadFail, MonadQuasi)

-- class MonadQuasi m where
--   liftQ :: Q a -> m a

-- instance MonadQuasi Q where
--   liftQ = id

-- instance (Monad m, MonadQuasi m) => MonadQuasi (ReaderT r m) where
--   liftQ = lift . liftQ

-- instance (Monad m, MonadQuasi m) => MonadQuasi (StateT r m) where
--   liftQ = lift . liftQ
