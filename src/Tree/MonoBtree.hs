{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}

module Tree.MonoBtree (module MonoBtreeCoq, deserialize, serialize)
where

import MonoBtreeCoq

import Data.Binary
import Data.Binary.Get (Get)
import qualified Data.Binary.Put
import Data.List (find, isPrefixOf, stripPrefix, last)
import Data.Maybe (fromJust, isJust)
import GHC.Generics (Generic)

deriving instance Show Key
deriving instance Generic Key
instance Binary Key
deriving instance Show a => Show (Tree a)

instance Binary a => Binary (Tree a) where
  put (Leaf l) = do
    put False
    put l
  put (InternalNode l) = do
    put True
    put l
  get = do
    isNode <- get
    if isNode then do
      children <- get
      return $ InternalNode children
    else do
      values <- get
      return $ Leaf values

serialize tr = put tr
deserialize :: Binary a => Get (Tree a)
deserialize = get
