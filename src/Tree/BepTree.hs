{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}

module Tree.BepTree (module BepTreeCoq, deserialize, serialize)
where

import BepTreeCoq

import Data.Binary
import Data.Binary.Get (Get)
import qualified Data.Binary.Put
import Data.List (find, isPrefixOf, stripPrefix, last)
import Data.Maybe (fromJust, isJust)
import GHC.Generics (Generic)

deriving instance Show Key
deriving instance Show Comparison
deriving instance Generic Key
deriving instance Generic a => Generic (JournalEntry a)
instance Binary Key
instance Generic a => Binary (JournalEntry a)
deriving instance Show a => Show (Tree a)
deriving instance Show a => Show (JournalEntry a)
deriving instance Show a => Show (WriteStatus a)
deriving instance (Show b) => Show (ReadStatus b)



instance Binary a => Binary (Tree a) where
  put (Leaf id j l) = do
    put False
    put id
    put j
    put l
  put (InternalNode id j l) = do
    put True
    put id
    put j
    put l
  get = do
    isNode <- get
    id <- get
    j <- get
    if isNode then do
      children <- get
      return $ InternalNode id j children
    else do
      values <- get
      return $ Leaf id j values

serialize tr = put tr
deserialize :: Binary a => Get (Tree a)
deserialize = get
