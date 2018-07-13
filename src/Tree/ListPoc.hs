{-# LANGUAGE DeriveGeneric #-}

module Tree.ListPoc
where

import Data.Binary
import Data.List (find, isPrefixOf, stripPrefix, last)
import Data.Maybe (fromJust, isJust)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get
import qualified Data.Binary.Put
import Debug.Trace (trace)
import GHC.Generics (Generic)

data Key =
    Meta String
  | Data String
  deriving (Eq, Show, Generic)
instance Binary Key

keyPath :: Key -> String
keyPath key = case key of
  Meta s -> s
  Data s -> s

onKeyPath :: (String -> String) -> Key -> Key
onKeyPath op k = case k of
  Meta s -> Meta (op s)
  Data s -> Data (op s)


writeErr :: String -> IO ()
writeErr str =
  appendFile "/tmp/err" (str ++ "\n")

type Tree t = [(Key, t)]

empty = []

add :: Tree t -> Key -> t -> Tree t
add t k v = (k, v):t

update :: Tree t -> Key -> t -> Tree t
update t k v = add (remove t k) k v

remove :: Tree t -> Key -> Tree t
remove t k = filter (\(k', _) -> k /= k') t

lookup :: Tree t -> Key -> Maybe t
lookup t k = snd <$> find (\(k', _) -> k == k') t

children :: Tree t -> Key -> [Key]
children t prefix =
  let prefixSlash = case last $ keyPath prefix of
        '/' -> keyPath prefix
        _ -> keyPath prefix ++ "/"
      getSuffix :: Key -> Maybe Key
      getSuffix mpath = case mpath of
        Meta path -> case stripPrefix prefixSlash path of
          Nothing -> Nothing
          Just s -> Just $ Meta s
        Data path -> case stripPrefix prefixSlash path of
          Nothing -> Nothing
          Just s -> Just $ Data s
      descendents :: [Key]
      descendents = map fromJust $ filter isJust $ map getSuffix $ map fst t
  in
  filter (all (/= '/') . keyPath) descendents

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
