module Store.Layered
where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Control.Exception (catch)
import Control.Concurrent.MVar
import Data.List (sort)
import System.Directory (getDirectoryContents)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

{-# NOINLINE lock #-}
lock :: MVar unit
lock = unsafePerformIO newEmptyMVar

takeLock = putMVar lock ()
releaseLock = takeMVar lock

readNode :: String -> Int -> IO BL.ByteString
readNode prefix id =
  BL.fromStrict <$> B.readFile (prefix ++ "/" ++ show id)

writeNode :: String -> Int -> BL.ByteString -> IO ()
writeNode prefix id contents = do
  BL.writeFile (prefix ++ "/" ++ show id) contents
  takeLock
  nextIdFile <- (BL.fromStrict <$> B.readFile (prefix ++ "/nextId")) `catch` ((\_ -> return $ BL8.pack "1") :: IOError -> IO BL.ByteString)
  case BL8.readInt nextIdFile of
    Nothing -> error "Can't read next id file"
    Just (nextId, _) -> do
      if nextId <= id then do
        BL.writeFile (prefix ++ "/nextId") (BL8.pack $ show $ id + 1)
        releaseLock
      else
        releaseLock

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
  takeLock
  nextIdFile <- (BL.fromStrict <$> B.readFile (prefix ++ "/nextId")) `catch` ((\_ -> return $ BL8.pack "1") :: IOError -> IO BL.ByteString)
  case BL8.readInt nextIdFile of
    Nothing -> error "Can't read next id file"
    Just (nextId, _) -> do
      BL.writeFile (prefix ++ "/nextId") (BL8.pack $ show $ nextId + 1)
      releaseLock
      return nextId
