module Main where

import Control.Exception.Base
import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

import System.Fuse

exnHandler :: SomeException -> IO Errno
exnHandler exc = return eNOENT

main :: IO ()
main = fuseMain fsOps exnHandler

fsOps :: FuseOperations ()
fsOps = defaultFuseOps { fuseGetFileStat = fsGetFileStat,
                         fuseOpen = fsOpen,
                         fuseRead = fsRead,
                         fuseOpenDirectory = fsOpenDir,
                         fuseReadDirectory = fsReadDir,
                         fuseGetFileSystemStats = fsGetFileSystemStats
                       }


fileContents :: B.ByteString
fileContents = B.pack "It works!\n"
fsPaths :: IO [FilePath]
fsPaths = readFile "/tmp/filelist" >>= return . lines


dirStat ctx = FileStat { statEntryType = Directory,
                         statFileMode = foldr1 unionFileModes [ ownerReadMode, ownerExecuteMode,
                                                                groupReadMode, groupExecuteMode,
                                                                otherReadMode, otherExecuteMode ],
                         statLinkCount = 2,
                         statFileOwner = fuseCtxUserID ctx,
                         statFileGroup = fuseCtxGroupID ctx,
                         statSpecialDeviceID = 0,
                         statFileSize = 4096,
                         statBlocks = 1,
                         statAccessTime = 0,
                         statModificationTime = 0,
                         statStatusChangeTime = 0
                       }

fileStat ctx = FileStat { statEntryType = RegularFile,
                          statFileMode = foldr1 unionFileModes [ ownerReadMode, groupReadMode, otherReadMode],
                          statLinkCount = 1,
                          statFileOwner = fuseCtxUserID ctx,
                          statFileGroup = fuseCtxGroupID ctx,
                          statSpecialDeviceID = 0,
                          statFileSize = fromIntegral $ B.length fileContents,
                          statBlocks = 1,
                          statAccessTime = 0,
                          statModificationTime = 0,
                          statStatusChangeTime = 0
                        }

fsGetFileStat "/" = do
  ctx <- getFuseContext
  return $ Right $ dirStat ctx
fsGetFileStat path = do
  paths <- fsPaths
  if path `elem` paths then do
    ctx <- getFuseContext
    return $ Right $ fileStat ctx
  else
    return $ Left eNOENT

fsOpenDir "/" = return eOK
fsOpenDir _   = return eNOENT

fsReadDir :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
fsReadDir "/" = do
  ctx <- getFuseContext
  paths <- fsPaths
  return $ Right $ [(".", dirStat ctx),
                    ("..", dirStat ctx)] ++
                   map (\path -> (tail path, fileStat ctx)) paths
fsReadDirectory _ = return (Left (eNOENT))

fsOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno ())
fsOpen path mode flags = do
  paths <- fsPaths
  if path `elem` paths then
    case mode of
      ReadOnly -> return $ Right ()
      _ -> return $ Left eACCES
  else
    return $ Left eNOENT

fsRead :: FilePath -> () -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
fsRead path () byteCount offset = do
  paths <- fsPaths
  if path `elem` paths then
    return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) fileContents
  else
    return $ Left eNOENT

fsGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
fsGetFileSystemStats str =
  return $ Right $ FileSystemStats {
    fsStatBlockSize = 512,
    fsStatBlockCount = 1,
    fsStatBlocksFree = 1,
    fsStatBlocksAvailable = 1,
    fsStatFileCount = 5,
    fsStatFilesFree = 10,
    fsStatMaxNameLength = 255
  }
