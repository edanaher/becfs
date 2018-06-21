module Main where

import Control.Exception.Base
import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import Foreign.C.Types
import System.Directory (renameFile)
import System.IO
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

import System.Fuse

exnHandler :: SomeException -> IO Errno
exnHandler exc = return eNOENT

main :: IO ()
main = fuseMain fsOps exnHandler

writeErr :: String -> IO ()
writeErr str =
  writeFile "/tmp/err" (str ++ "\n")

fsOps :: FuseOperations ()
fsOps = defaultFuseOps { fuseGetFileStat = fsGetFileStat,
                         fuseOpen = fsOpen,
                         fuseRead = fsRead,
                         fuseWrite = fsWrite,
                         fuseCreateDevice = fsCreateDevice,
                         fuseRemoveLink = fsRemoveLink,
                         fuseRename = fsRename,
                         fuseSetFileSize = fsSetFileSize,
                         fuseOpenDirectory = fsOpenDir,
                         fuseReadDirectory = fsReadDir,
                         fuseFlush = fsFlush,
                         fuseRelease = fsRelease,
                         fuseSynchronizeFile = fsSynchronizeFile,
                         fuseGetFileSystemStats = fsGetFileSystemStats,
                         fuseSetFileTimes = fsSetFileTimes,
                         fuseSetFileMode = fsSetFileMode,
                         fuseAccess = fsAccess
                       }


fileContents :: FilePath -> IO B.ByteString
fileContents path = do
  allFiles <- readFile "/tmp/filelist"
  line <- return $ head $ filter ((== path) . head . words) $ lines allFiles
  return $ B.pack $ unwords $ tail $ words line

removeFile :: FilePath -> IO ()
removeFile path = do
  allFiles <- readFile "/tmp/filelist"
  otherfiles <- return $ filter ((/= path) . head . words) $ lines allFiles
  writeFile "/tmp/filelist2" $ unlines $ otherfiles
  renameFile "/tmp/filelist2" "/tmp/filelist"
  return ()

writeToFile :: FilePath -> B.ByteString -> FileOffset -> IO ()
writeToFile path rawContents offset = do
  contents <- return $ B.intercalate (B.pack "::") $ B.split '\n' rawContents
  allFiles <- readFile "/tmp/filelist"
  otherfiles <- return $ filter ((/= path) . head . words) $ lines allFiles
  writeFile "/tmp/filelist2" $ unlines $ (path ++ " " ++ B.unpack contents):otherfiles
  renameFile "/tmp/filelist2" "/tmp/filelist"
  return ()

moveFile :: FilePath -> FilePath -> IO ()
moveFile from to = do
  allFiles <- readFile "/tmp/filelist"
  contents <- return $ unwords $ tail $ words $ head $ filter ((== from) . head . words) $ lines allFiles
  otherfiles <- return $ filter ((/= from) . head . words) $ lines allFiles
  writeFile "/tmp/filelist2" $ unlines $ (to ++ " " ++ contents):otherfiles
  renameFile "/tmp/filelist2" "/tmp/filelist"
  return ()


fsPaths :: IO [FilePath]
fsPaths = readFile "/tmp/filelist" >>= return . map (head . words) . lines


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

fileStat content ctx = FileStat { statEntryType = RegularFile,
                                  statFileMode = foldr1 unionFileModes [ ownerReadMode, ownerWriteMode, groupReadMode, otherReadMode],
                                  statLinkCount = 1,
                                  statFileOwner = fuseCtxUserID ctx,
                                  statFileGroup = fuseCtxGroupID ctx,
                                  statSpecialDeviceID = 0,
                                  statFileSize = fromIntegral $ B.length $ content,
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
    content <- fileContents path
    return $ Right $ fileStat content ctx
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
                   map (\path -> (tail path, fileStat (B.pack "haha") ctx)) paths
fsReadDirectory _ = return (Left (eNOENT))

fsOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno ())
fsOpen path mode flags = do
  paths <- fsPaths
  if path `elem` paths then
    case mode of
      ReadOnly -> return $ Right ()
      WriteOnly -> return $ Right ()
      _ -> return $ Left eACCES
  else
    return $ Right ()

fsRead :: FilePath -> () -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
fsRead path () byteCount offset = do
  paths <- fsPaths
  contents <- fileContents path
  if path `elem` paths then
    return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) contents
  else
    return $ Left eNOENT

fsWrite :: FilePath -> () -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
fsWrite path () bytes offset = do
  writeToFile path bytes offset
  return $ Right $ CSize $ fromIntegral $ B.length bytes

fsCreateDevice :: FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
fsCreateDevice path tp mode id = case tp of
  RegularFile -> writeToFile path B.empty 0 >> return eOK
  _ -> return eNOSYS

fsRemoveLink :: FilePath -> IO Errno
fsRemoveLink path =
  removeFile path >> return eOK

fsRename :: FilePath -> FilePath -> IO Errno
fsRename from to =
  moveFile from to >> return eOK

fsSetFileSize :: FilePath -> FileOffset -> IO Errno
fsSetFileSize path size = return eOK


fsFlush :: FilePath -> () -> IO Errno
fsFlush path () = return eOK

fsRelease :: FilePath -> () -> IO ()
fsRelease path () = return ()

fsSynchronizeFile :: FilePath -> SyncType -> IO Errno
fsSynchronizeFile path synctype = return eOK

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

fsSetFileTimes :: FilePath -> EpochTime -> EpochTime -> IO Errno
fsSetFileTimes path t1 t2 = return eOK

fsSetFileMode :: FilePath -> FileMode -> IO Errno
fsSetFileMode path mode = return eOK

fsAccess :: FilePath -> Int -> IO Errno
fsAccess str uid = return eOK
