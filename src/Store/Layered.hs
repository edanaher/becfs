module Store.Layered
where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List (sort)
import System.Directory (getDirectoryContents)
import Text.Read (readMaybe)

readNode :: String -> Int -> IO BL.ByteString
readNode prefix id =
  BL.fromStrict <$> B.readFile (prefix ++ "/" ++ show id)

writeNode :: String -> Int -> BL.ByteString -> IO ()
writeNode prefix id contents =
  BL.writeFile (prefix ++ "/" ++ show id) contents

writeRoot :: String -> Int -> BL.ByteString -> IO ()
writeRoot prefix id contents = do
  writeNode prefix id contents
  BL.writeFile (prefix ++ "/root") (BL8.pack $ show id)

readRoot :: String -> IO BL.ByteString
readRoot prefix = do
  rootFile <- BL.fromStrict <$> B.readFile (prefix ++ "/root")
  case BL8.readInt rootFile of
    Nothing -> error "Can't read root id file"
    Just (rootId, _) -> readNode prefix rootId

nextId :: String -> IO Int
nextId prefix = do
  contents <- getDirectoryContents prefix
  final <- return $ last $ sort $ map (\f ->
        case readMaybe f of
          Nothing -> -1
          Just n -> n) contents
  return $ final + 1

