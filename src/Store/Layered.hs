module Store.Layered
where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import System.Directory (getDirectoryContents)
import Text.Read (readMaybe)

readNode :: String -> Int -> IO BL.ByteString
readNode prefix id =
  BL.fromStrict <$> B.readFile (prefix ++ "/" ++ show id)

writeNode :: String -> Int -> BL.ByteString -> IO ()
writeNode prefix id contents =
  BL.writeFile (prefix ++ "/" ++ show id) contents

nextId :: String -> IO Int
nextId prefix = do
  contents <- getDirectoryContents prefix
  final <- return $ last $ sort $ map (\f ->
        case readMaybe f of
          Nothing -> -1
          Just n -> n) contents
  return $ final + 1

