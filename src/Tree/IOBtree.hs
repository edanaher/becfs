{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}

module Tree.IOBtree (module IOBtreeCoq, deserialize, serialize)
where

import IOBtreeCoq

import Data.Binary
import Data.Binary.Get (Get)
import qualified Data.Binary.Put
import Data.List (find, isPrefixOf, stripPrefix, last)
import Data.Maybe (fromJust, isJust)
import GHC.Generics (Generic)

deriving instance Show Key
deriving instance Show Comparison
deriving instance Generic Key
instance Binary Key
deriving instance Show a => Show (IOTree a)
deriving instance Show a => Show (WriteStatus a)
deriving instance (Show b) => Show (ReadStatus b)

instance Binary a => Binary (IOTree a) where
  put (Leaf id l) = do
    put False
    put id
    put l
  put (InternalNode id l) = do
    put True
    put id
    put l
  get = do
    isNode <- get
    id <- get
    if isNode then do
      children <- get
      return $ InternalNode id children
    else do
      values <- get
      return $ Leaf id values

serialize tr = put tr
deserialize :: Binary a => Get (IOTree a)
deserialize = get
