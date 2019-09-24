{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}

module Tree.Btree (module BtreeCoq, deserialize, serialize)
where

import BtreeCoq

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
deriving instance Show a => Show (Tree a)
deriving instance Show a => Show (WriteStatus a)
deriving instance (Show a, Show b) => Show (ReadStatus a b)

instance Binary a => Binary (Tree a) where
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
deserialize :: Binary a => Get (Tree a)
deserialize = get
