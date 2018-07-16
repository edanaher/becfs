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

serialize :: Binary t => Tree t -> Put
serialize (Leaf []) = put False
serialize (Leaf ((k,v):r)) = do
  put True
  put k
  put v
  serialize (Leaf r)
serialize (InternalNode _) = put False

deserialize :: Show t => Binary t => Get (Tree t)
deserialize =
  let deserializeLeaf = do
        hasMore <- get
        if hasMore then do
          k <- get
          v <- get
          t' <- deserializeLeaf
          return ((k, v):t')
        else
          return []
   in
     Leaf <$> deserializeLeaf
