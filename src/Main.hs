module Main where

import Control.Exception.Base
import Data.Binary (Binary)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Foreign.C.Error
import Foreign.C.Types
import System.Directory (renameFile)
import System.IO
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

import System.Fuse

import Tree.ListPoc as Tree

metadatafile = "/tmp/becfs-metadata"
metadatafile2 = "/tmp/becfs-metadata.new"
datafile = "/tmp/becfs-data"
datafile2 = "/tmp/becfs-data.new"

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
                         fuseAccess = fsAccess,
                         fuseInit = fsInit
                       }


updateTree :: Binary a => FilePath -> FilePath -> (Tree a -> Tree a) -> IO ()
updateTree treeFile bakFile op = do
  bytes <- BL.readFile treeFile
  tree <- return $ (runGet Tree.deserialize bytes)
  tree' <- return $ op tree
  bytes' <- return $ runPut $ Tree.serialize tree'
  BL.writeFile bakFile bytes'
  renameFile bakFile treeFile

fileContents :: FilePath -> IO (Maybe B.ByteString)
fileContents path = do
  return $ B.pack ""
  allContents <- BL.readFile datafile
  contentTree <- return $ runGet Tree.deserialize allContents
  return $ Tree.lookup contentTree path
--  line <- return $ head $ filter ((== path) . head . words) $ lines allFiles
--  return $ B.pack $ unwords $ tail $ words line

removeFile :: FilePath -> IO ()
removeFile path = do
  updateTree metadatafile metadatafile2 (\t -> remove t path :: Tree ())

createFileMeta :: FilePath -> IO ()
createFileMeta path = do
  allMeta <- BL.readFile metadatafile
  tree <- return $ (runGet Tree.deserialize allMeta)
  tree' <- return $ Tree.add tree path ()
  allMeta' <- return $ runPut $ Tree.serialize tree'
  BL.writeFile metadatafile2 allMeta'
  renameFile metadatafile2 metadatafile

ensureFileExists :: FilePath -> IO ()
ensureFileExists path = do
  exists <- fileExists path
  if not exists then
    createFileMeta path
  else
    return ()

writeToFile :: FilePath -> B.ByteString -> FileOffset -> IO ()
writeToFile path contents offset = do
  ensureFileExists path
  allFiles <- BL.readFile datafile
  tree <- return $ runGet Tree.deserialize allFiles
  tree' <- return $ Tree.update tree path contents
  allFiles' <- return $ runPut $ Tree.serialize tree'
  BL.writeFile datafile2 allFiles'
  renameFile datafile2 datafile

moveFile :: FilePath -> FilePath -> IO ()
moveFile from to = do
  allMeta <- BL.readFile metadatafile
  tree <- return $ (runGet Tree.deserialize allMeta :: Tree ())
  tree' <- return $ Tree.add (Tree.remove tree from) to ()
  allMeta' <- return $ runPut $ Tree.serialize tree'
  BL.writeFile metadatafile2 allMeta'
  renameFile metadatafile2 metadatafile

fileExists :: FilePath -> IO Bool
fileExists path = do
  files <- fsPaths "/"
  return $ any (== path) files

fsPaths :: FilePath -> IO [FilePath]
fsPaths dir = do
  allMeta <- BL.readFile metadatafile
  tree <- return $ (runGet Tree.deserialize allMeta :: Tree ())
  return $ Tree.keysUnder tree "/"


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
  paths <- fsPaths "/"
  if path `elem` paths then do
    ctx <- getFuseContext
    content <- fileContents path -- This should read metadata
    case content of
      Nothing -> return $ Right $ fileStat (B.empty) ctx
      Just c -> return $ Right $ fileStat c ctx
  else
    return $ Left eNOENT

fsOpenDir "/" = return eOK
fsOpenDir _   = return eNOENT

fsReadDir :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
fsReadDir "/" = do
  ctx <- getFuseContext
  paths <- fsPaths "/"
  return $ Right $ [(".", dirStat ctx),
                    ("..", dirStat ctx)] ++
                   map (\path -> (tail path, fileStat (B.pack "haha") ctx)) paths
fsReadDirectory _ = return (Left (eNOENT))

fsOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno ())
fsOpen path mode flags = do
  paths <- fsPaths "/"
  if path `elem` paths then
    case mode of
      ReadOnly -> return $ Right ()
      WriteOnly -> return $ Right ()
      _ -> return $ Left eACCES
  else
    return $ Right ()

fsRead :: FilePath -> () -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
fsRead path () byteCount offset = do
  paths <- fsPaths "/"
  contents <- fileContents path
  if path `elem` paths then
    case contents of
      Nothing -> return $ Left eNOENT
      Just c -> return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) c
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

fsInit :: IO ()
fsInit = do
  BL.writeFile metadatafile $ runPut $ Tree.serialize (Tree.empty :: Tree ())
  BL.writeFile datafile $ runPut $ Tree.serialize (Tree.empty :: Tree B.ByteString)
