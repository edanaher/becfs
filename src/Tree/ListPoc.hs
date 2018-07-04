module Tree.ListPoc
where

import Data.Binary
import Data.List (find)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get
import qualified Data.Binary.Put
import Debug.Trace (trace)

import System.IO.Unsafe (unsafePerformIO)

writeErr :: String -> IO ()
writeErr str =
  appendFile "/tmp/err" (str ++ "\n")

type Tree t = [(String, t)]

empty = []

add :: Tree t -> String -> t -> Tree t
add t k v = (k, v):t

update :: Tree t -> String -> t -> Tree t
update = add

remove :: Tree t -> String -> Tree t
remove t k = filter (\(k', _) -> k /= k') t

lookup :: Tree t -> String -> Maybe t
lookup t k = snd <$> find (\(k', _) -> k == k') t

keysUnder :: Tree t -> String -> [String]
keysUnder t prefix = map fst t

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
