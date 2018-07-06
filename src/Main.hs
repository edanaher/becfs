{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Concurrent.MVar
import Control.Exception.Base
import Data.Binary (Binary)
import Data.Binary.Get (runGet, runGetOrFail)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock.POSIX (getPOSIXTime)
import Foreign.C.Error
import Foreign.C.Types
import GHC.Generics (Generic)
import System.Directory (renameFile)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

import System.Fuse

import qualified Tree.ListPoc as Tree
import Tree.ListPoc as Tree (Tree)

metadatafile = "/tmp/becfs-metadata"
metadatafile2 = "/tmp/becfs-metadata.new"
datafile = "/tmp/becfs-data"
datafile2 = "/tmp/becfs-data.new"

{-# NOINLINE lock #-}
lock :: MVar unit
lock = unsafePerformIO newEmptyMVar

takeLock = putMVar lock ()
releaseLock = takeMVar lock

exnHandler :: SomeException -> IO Errno
exnHandler exc = do
  writeErr $ "Exception: " ++ show exc
  return eNOENT

data Metadata =
    FileMetadata { filesize :: Int, mtime :: Int }
  | DirMetadata { mtime :: Int }
  deriving (Eq, Show, Generic)
instance Binary Metadata

getNow :: IO Int
getNow = round <$> getPOSIXTime


emptyFileMetadata :: IO Metadata
emptyFileMetadata = do
  now <- getNow
  return $ FileMetadata { filesize = 0, mtime = now }

zeroFileMetadata :: Metadata
zeroFileMetadata = FileMetadata { filesize = 0, mtime = 0 }

emptyDirMetadata :: IO Metadata
emptyDirMetadata = do
  now <- getNow
  return $ DirMetadata { mtime = now }

main :: IO ()
main = fuseMain fsOps exnHandler

{-# NOINLINE errlock #-}
errlock :: MVar unit
errlock = unsafePerformIO newEmptyMVar

writeErr :: String -> IO ()
writeErr str = do
  putMVar errlock ()
  appendFile "/tmp/err" (str ++ "\n")
  takeMVar errlock

fsOps :: FuseOperations ()
fsOps = defaultFuseOps { fuseGetFileStat = fsGetFileStat,
                         fuseOpen = fsOpen,
                         fuseRead = fsRead,
                         fuseWrite = fsWrite,
                         fuseCreateDevice = fsCreateDevice,
                         fuseCreateDirectory = fsCreateDirectory,
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


updateTree :: Show a => Binary a => FilePath -> FilePath -> (Tree a -> Tree a) -> IO ()
updateTree treeFile bakFile op = do
  takeLock
  bytes <- BL.fromStrict <$> B.readFile treeFile
  tree <- return $ (runGet Tree.deserialize bytes)
  tree' <- return $ op tree
  bytes' <- return $ runPut $ Tree.serialize tree'
  BL.writeFile bakFile bytes'
  renameFile bakFile treeFile
  releaseLock

getTree :: Show a => Binary a => FilePath -> IO (Tree a)
getTree treeFile = do
  takeLock
  bytes <- BL.fromStrict <$> B.readFile treeFile
  case runGetOrFail Tree.deserialize bytes of
    Left (_, _, err) -> do
      writeErr $ "Error reading tree: " ++ err
      releaseLock
      return Tree.empty
    Right (_, _, tree) -> do
      releaseLock
      return tree

fileContents :: FilePath -> IO (Maybe B.ByteString)
fileContents path = do
  tree <- getTree datafile
  return $ Tree.lookup tree path

removeFile :: FilePath -> IO ()
removeFile path = do
  updateTree metadatafile metadatafile2 (\t -> Tree.remove t path :: Tree Metadata)

createFileMeta :: FilePath -> IO ()
createFileMeta path =  do
  metadata <- emptyFileMetadata
  updateTree metadatafile metadatafile2 $ \t -> Tree.add t path metadata

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
  updateTree metadatafile metadatafile2 (\t ->
    let metadata = case Tree.lookup t path of
          Just m -> m
          Nothing -> zeroFileMetadata
    in
    Tree.update t path $ metadata { filesize = B.length contents })
  updateTree datafile datafile2 (\t -> Tree.update t path contents)

moveFile :: FilePath -> FilePath -> IO ()
moveFile from to = do
  updateTree metadatafile metadatafile2 (\t ->
      let metadata = case Tree.lookup t from of
            Just m -> m
            Nothing -> zeroFileMetadata
      in
      Tree.add (Tree.remove t from) to metadata)
  updateTree datafile datafile2 (\t ->
      let contents = case Tree.lookup t from of
            Just b -> b
            Nothing -> B.pack "Lost contents!"
      in
      Tree.add (Tree.remove t from) to contents)

fileExists :: FilePath -> IO Bool
fileExists path = do
  files <- fsPaths "/"
  return $ any (== path) files

fsPaths :: FilePath -> IO [FilePath]
fsPaths dir = do
  tree <- (getTree metadatafile :: IO (Tree Metadata))
  return $ Tree.keysUnder tree "/"


dirStat metadata ctx = FileStat {
    statEntryType = Directory,
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
    statModificationTime = fromIntegral $ mtime metadata,
    statStatusChangeTime = 0
  }

fileStat metadata ctx = FileStat {
    statEntryType = RegularFile,
    statFileMode = foldr1 unionFileModes [ ownerReadMode, ownerWriteMode, groupReadMode, otherReadMode],
    statLinkCount = 1,
    statFileOwner = fuseCtxUserID ctx,
    statFileGroup = fuseCtxGroupID ctx,
    statSpecialDeviceID = 0,
    statFileSize = fromIntegral $ filesize metadata,
    statBlocks = 1,
    statAccessTime = 0,
    statModificationTime = fromIntegral $ mtime metadata,
    statStatusChangeTime = 0
  }

fsGetFileStat "/" = do
  writeErr $ "Statting /"
  ctx <- getFuseContext
  metadata <- emptyDirMetadata
  return $ Right $ dirStat metadata ctx
fsGetFileStat path = do
  writeErr $ "Statting " ++ show path
  tree <- getTree metadatafile
  writeErr $ "  statting " ++ show path ++ ": loaded tree"
  case Tree.lookup tree path of
    Just metadata -> do
      writeErr $ "  statting " ++ show path ++ " => " ++ show metadata
      ctx <- getFuseContext
      case metadata of
        FileMetadata{} -> return $ Right $ fileStat metadata ctx
        DirMetadata{} -> return $ Right $ dirStat metadata ctx
    Nothing -> return $ Left eNOENT

fsOpenDir _ = return eOK

fsReadDir :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
fsReadDir dir = do
  ctx <- getFuseContext
  metadata <- emptyDirMetadata
  tree <- getTree metadatafile
  writeErr $ "Reading dir " ++ show dir ++ " => " ++ show (Tree.children tree dir)
  val <- return $ Right $ [(".", dirStat metadata ctx),
                    ("..", dirStat metadata ctx)] ++
                   map (\path -> case Tree.lookup tree (dir ++ path) of
                       Nothing -> (path, fileStat zeroFileMetadata ctx)
                       Just md -> case md of
                         FileMetadata{} -> (path, fileStat md ctx)
                         DirMetadata{} -> (path, dirStat md ctx)
                       ) (Tree.children tree dir)
  writeErr $ "  read dir " ++ show dir ++ " => " ++ (case val of Right dirs -> show dirs; Left _ -> "ERROR")
  return val

fsOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno ())
fsOpen path mode flags = do
  writeErr $ "Opening path " ++ show path
  paths <- fsPaths "/"
  if path `elem` paths then do
    writeErr $ "Opening path " ++ show path ++ " in " ++ show paths
    case mode of
      ReadOnly -> return $ Right ()
      WriteOnly -> return $ Right ()
      _ -> return $ Left eACCES
  else do
    writeErr $ "Failed to open path " ++ show path ++ " in " ++ show paths
    return $ Right ()

fsRead :: FilePath -> () -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
fsRead path () byteCount offset = do
  writeErr $ "Reading path " ++ show path
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
  writeErr $ "Writing path " ++ show path
  writeToFile path bytes offset
  return $ Right $ CSize $ fromIntegral $ B.length bytes

fsCreateDevice :: FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
fsCreateDevice path tp mode id = case tp of
  RegularFile -> do
    writeErr $ "Creating file " ++ show path
    writeToFile path B.empty 0
    return eOK
  _ -> return eNOSYS

fsCreateDirectory :: FilePath -> FileMode -> IO Errno
fsCreateDirectory path mode =  do
  writeErr $ "Creating directory " ++ show path
  metadata <- emptyDirMetadata
  updateTree metadatafile metadatafile2 (\t -> Tree.add t path metadata)
  return eOK

fsRemoveLink :: FilePath -> IO Errno
fsRemoveLink path = do
  writeErr $ "Unlinking " ++ show path
  removeFile path
  return eOK

fsRename :: FilePath -> FilePath -> IO Errno
fsRename from to = do
  writeErr $ "Renaming " ++ show from ++ " => " ++ show to
  moveFile from to
  return eOK

fsSetFileSize :: FilePath -> FileOffset -> IO Errno
fsSetFileSize path size = do
  writeErr $ "Resizing " ++ show path ++ " => " ++ show size
  return eOK


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
  BL.writeFile metadatafile $ runPut $ Tree.serialize (Tree.empty :: Tree Metadata)
  BL.writeFile datafile $ runPut $ Tree.serialize (Tree.empty :: Tree B.ByteString)
