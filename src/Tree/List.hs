{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}

module Tree.List (module ListCoq, deserialize, serialize)
where

import ListCoq

import Data.Binary
import Data.Binary.Get (Get)
import qualified Data.Binary.Put
import Data.List (find, isPrefixOf, stripPrefix, last)
import Data.Maybe (fromJust, isJust)
import GHC.Generics (Generic)

deriving instance Show Key
deriving instance Generic Key
instance Binary Key

serialize :: Binary t => Tree t -> Put
serialize [] = put False
serialize ((k,v):r) = do
  put True
  put k
  put v
  serialize r

deserialize :: Show t => Binary t => Get (Tree t)
deserialize = do
  hasMore <- get
  if hasMore then do
    k <- get
    v <- get
    t' <- deserialize
    return ((k, v):t')
  else
    return []
