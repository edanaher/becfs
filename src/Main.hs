{-# LANGUAGE DeriveGeneric, StandaloneDeriving, ScopedTypeVariables #-}

module Main where

import Control.Concurrent.MVar
import Control.Exception.Base
import Data.Binary (Binary)
import Data.Binary.Get (runGet, runGetOrFail)
import Data.Binary.Put (runPut)
import qualified Data.ByteString as By
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

import qualified Tree.Btree as Tree
import Tree.Btree (Tree)
import qualified Tree.IOBtree as IOTree
import Tree.IOBtree (IOTree)
import qualified Store.Layered as Store

deriving instance Show Errno

{-# NOINLINE lock #-}
lock :: MVar unit
lock = unsafePerformIO newEmptyMVar

takeLock = putMVar lock ()
releaseLock = takeMVar lock

exnHandler :: SomeException -> IO Errno
exnHandler exc = do
  writeErr $ "Exception: " ++ show exc
  return eDEADLK

--instance Binary CMode where
--  put (CMode x) = put x
--  get = CMode .get
deriving instance Generic CMode
instance Binary CMode
deriving instance Generic CUid
instance Binary CUid
deriving instance Generic CGid
instance Binary CGid

data TreeValue =
    FileMetadata { filesize :: Int, perms :: FileMode, owner :: UserID, group :: GroupID, mtime :: Int }
  | DirMetadata { mtime :: Int }
  | SymlinkMetadata { mtime :: Int, target :: FilePath }
  | Data B.ByteString
  deriving (Eq, Show, Generic)
instance Binary TreeValue

getNow :: IO Int
getNow = round <$> getPOSIXTime

appendPath :: [FilePath] -> FilePath
appendPath [] = ""
appendPath [t] = t
appendPath (h:r) =
  if last h == '/'
  then h ++ appendPath r
  else h ++ "/" ++ appendPath r

dummyFileMetadata :: Int -> IO TreeValue
dummyFileMetadata size = do
  now <- getNow
  ctx <- getFuseContext
  return $ FileMetadata {
    filesize = size,
    mtime = now,
    owner = fuseCtxUserID ctx,
    group = fuseCtxGroupID ctx,
    perms = CMode $ fromIntegral 0o644
  }

emptyFileMetadata :: IO TreeValue
emptyFileMetadata = dummyFileMetadata 0

emptyDirMetadata :: IO TreeValue
emptyDirMetadata = do
  now <- getNow
  return $ DirMetadata { mtime = now }

treeToIOTree :: Tree t -> IOTree t
treeToIOTree tr =
  let fixKey (Tree.Meta x) = IOTree.Meta x
      fixKey (Tree.Data x) = IOTree.Data x
      fixKeys = map (\(k, v) -> (fixKey k, v))
  in case tr of
    Tree.Leaf id vs -> IOTree.Leaf id $ fixKeys vs
    Tree.InternalNode id vs -> IOTree.InternalNode id $ fixKeys vs

main :: IO ()
main = do
  --testStore
  testBtree
  --testFs
  --fuseMain fsOps exnHandler

extractTreeFromStatus :: Tree.WriteStatus t -> Tree t
extractTreeFromStatus tr = case tr of
  Tree.WriteSuccess root _ -> root
  _ -> error "Not WriteSuccess"

writeNode :: Binary t => Tree t -> IO ()
writeNode tr =
  let id = case tr of
        Tree.Leaf id _ -> id
        Tree.InternalNode id _ -> id in
  Store.writeNode "/tmp/becfs" id (runPut $ Tree.serialize tr)

testFs :: IO ()
testFs = do
  writeToFile "/file1" (B.pack "This is file1") 0
  writeToFile "/dir1/file2" (B.pack "This is file2") 0
  writeToFile "/dir2/dir2/file3" (B.pack "This is file3") 0
  writeToFile "/dir1/file1a" (B.pack "This is file1a") 0
  writeToFile "/file2b" (B.pack "This is file2b") 0
  writeToFile "/dir1/file99" (B.pack "This is file99") 0
  writeToFile "/dir1/file94" (B.pack "This is file94") 0
  writeToFile "/dir2/dir2/file90" (B.pack "This is file90") 0
  writeToFile "/dir1/file50" (B.pack "This is file50") 0
  writeToFile "/dir3/file58" (B.pack "This is file58") 0
  writeToFile "/dir2/dir2/file54" (B.pack "This is file54") 0
  writeToFile "/dir3/file55" (B.pack "This is file55") 0
  printBtreeFromDisk "test finished: "
  fsOpen "/dir3/file55" ReadOnly defaultFileFlags >>= print
  fsOpen "/filedoesnotexist" ReadOnly defaultFileFlags >>= print
  print =<< fsRead  "/dir3/file55" () 100 0
  print =<< fsRead  "/dir2/dir2/file3" () 100 0
  print =<< fsRead  "/file3doesnotexist" () 100 0
  root <- fetchRootNode :: IO (Tree TreeValue)
  print =<< childEntries root "/dir2/"
  print =<< childEntries root "/dir2/dir2/"
  print =<< childEntries root "/dir1/"


printBtree :: String -> Tree t -> IO ()
printBtree prefix tr =
  let aux :: String -> Tree t -> IO ()
      aux indent tr = case tr of
        Tree.Leaf id ns -> do
          flip mapM_ ns (\(key, _) -> putStr $ indent ++ show key ++ "\n")
        Tree.InternalNode id ns -> do
          flip mapM_ ns (\(minKey, nodeId) -> do
              putStr $ indent ++ show nodeId ++ ": " ++ show minKey ++ "\n"
              bytes <- Store.readNode "/tmp/becfs" nodeId
              tree' <- return $ runGet Tree.deserialize $ bytes :: IO (Tree TreeValue)
              aux (indent ++ "  ") tree'
            )
  in
  aux prefix tr

showBtree :: String -> IO String
showBtree prefix =
  let aux :: String -> Tree t -> IO [String]
      aux indent tr = case tr of
        Tree.Leaf id ns -> do
          flip mapM ns (\(key, _) -> return $ indent ++ show key ++ "\n")
        Tree.InternalNode id ns -> do
          return . concat =<< flip mapM ns (\(minKey, nodeId) -> do
              bytes <- Store.readNode "/tmp/becfs" nodeId
              tree' <- return $ runGet Tree.deserialize $ bytes :: IO (Tree TreeValue)
              otherNodes <- aux (indent ++ "  ") tree'
              me <- return $ indent ++ show nodeId ++ ": " ++ show minKey ++ "\n"
              return $ me:otherNodes)
  in do
  tr <- fetchRootNode :: IO (Tree TreeValue)
  return . ((show (Tree.getNodeId tr) ++ ":\n") ++) . concat =<< aux (prefix ++ "  ") tr

printBtreeFromDisk :: String -> IO ()
printBtreeFromDisk prefix = do
  tree <- fetchRootNode :: IO (Tree TreeValue)
  printBtree prefix tree

testBtree :: IO ()
testBtree =
  let addNode tr n = do
        Tree.WriteSuccess tr' newNodes <- insertMeta tr ("/file" ++ n)  ("Metadata for " ++ show n) :: IO (Tree.WriteStatus String)
        root_bytes <- return $ runPut $ Tree.serialize tr'
        Store.writeRoot "/tmp/becfs" (Tree.getNodeId tr') root_bytes
        mapM_ writeNode newNodes
        putStr $ "Tree+" ++ n ++ ": " ++ show tr' ++ " / " ++ show newNodes ++ "\n"
        return tr'
      readMeta k = do
        root <- fetchIORootNode :: IO (IOTree String)
        readResult <- IOTree.lookup (\str -> putStrLn $ "Inside coq!" ++ str) root (IOTree.Meta k)
        case readResult of
          IOTree.ReadSuccess v -> putStr $ "Read " ++ k ++ " => " ++ show v ++ "\n"
          IOTree.ReadDoesNotExist -> putStr $ "Read " ++ k ++ " DOESNOTEXIST" ++ "\n"
          IOTree.ReadError str -> putStr $ "Read " ++ k ++ " ERROR: " ++ str ++ "\n"
  in do
  tree <- return Tree.empty
  tree1 <- addNode tree "3"
  tree2 <- addNode tree1 "1"
  tree3 <- addNode tree2 "2"
  readMeta "/file1"
{-readMeta tree3 "/file2"
  readMeta tree3 "/file3"
  readMeta tree3 "/file4"
  tree4 <- addNode tree3 "4"
  readMeta tree4 "/file1"
  tree5 <- addNode tree4 "1a"
  tree6 <- addNode tree5 "6"
  tree7 <- addNode tree6 "8"
  tree8 <- addNode tree7 "7"
  tree9 <- addNode tree8 "11"
  tree10 <- addNode tree9 "13"
  tree11 <- addNode tree10 "14"
  tree12 <- addNode tree11 "15"
  tree13 <- addNode tree12 "16"
  tree14 <- addNode tree13 "17"
  tree15 <- addNode tree14 "18"
  tree16 <- addNode tree15 "19"
  tree17 <- addNode tree16 "20"
  tree18 <- addNode tree17 "21"
  tree19 <- addNode tree18 "22"
  tree20 <- addNode tree19 "23"
  tree21 <- addNode tree20 "94"
  tree22 <- addNode tree21 "85"
  tree23 <- addNode tree22 "76"
  tree24 <- addNode tree23 "67"
  tree25 <- addNode tree24 "58"
  --printBtree "" tree25
  readMeta tree25 "/file58"
  readMeta tree25 "/file11"
  readMeta tree25 "/file94"
  readMeta tree25 "/file9499"
  putStr "Success\n"-}

testStore :: IO ()
testStore = do
  Store.writeNode "/tmp/becfs" 1 (BL.fromStrict $ B.pack "hi there")
  show <$> Store.readNode "/tmp/becfs" 1 >>= print
  Store.nextId "/tmp/becfs" >>= print . show
  Store.writeNode "/tmp/becfs" 4 (BL.fromStrict $ B.pack "hi there2")
  Store.nextId "/tmp/becfs" >>= print . show

iterateOpUntilSuccess :: Binary t => Show t => (Tree t -> Tree.Key -> t -> [Tree t] -> [Int] -> Tree.WriteStatus t) -> Tree t -> Tree.Key -> t -> IO (Tree.WriteStatus t)
iterateOpUntilSuccess op tr k v =
  let --aux :: [Int] -> [Tree t] -> Int -> IO (Tree.WriteStatus t)
      aux freshIds nodes nextId = do
        attempt <- return $ op tr k v nodes freshIds
        --putStrLn $ "Adding " ++ show k ++ ": " ++ show attempt
        case attempt of
          Tree.WriteSuccess _ _ -> return attempt
          Tree.WriteNeedFreshId n -> do
            aux (freshIds ++ [nextId + i | i <- [0..n-1]]) nodes (nextId + n)
          Tree.WriteNeedNode id -> do
            bytes <- Store.readNode "/tmp/becfs" id
            newNode <- return $ runGet Tree.deserialize bytes
            aux freshIds (nodes ++ [newNode]) nextId
          Tree.WriteError err -> error $ "Error inserting meta: " ++ err
          other -> error $ "Unexpected WriteStatus: " ++ show other
  in do
  nextId <- Store.nextId "/tmp/becfs" -- RACE CONDITION; need to record that these are allocated.
  writeErr $ "next id is " ++ show nextId
  aux [] [] nextId

insertMeta :: Binary t => Show t => Tree t -> String -> t -> IO (Tree.WriteStatus t)
insertMeta tr k v =
  iterateOpUntilSuccess Tree.insert tr (Tree.Meta k) v

insertData :: Binary t => Show t => Tree t -> String -> t -> IO (Tree.WriteStatus t)
insertData tr k v =
  iterateOpUntilSuccess Tree.insert tr (Tree.Data k) v

updateNode :: Binary t => Show t => Tree t -> Tree.Key -> t -> IO (Tree.WriteStatus t)
updateNode tr k v = do
  Tree.WriteSuccess rt ns <- iterateOpUntilSuccess (\tr k v ns ids -> Tree.remove tr k ns ids) tr k v
  tree <- loadRoot "/tmp/becfs"

  root_bytes <- return $ runPut $ Tree.serialize rt
  Store.writeRoot "/tmp/becfs" (Tree.getNodeId rt) root_bytes
  flip mapM_ ns $ \node -> do
      bytes <- return $ runPut $ Tree.serialize node
      writeErr $ "writing node " ++ (show $ Tree.getNodeId node)
      Store.writeNode "/tmp/becfs" (Tree.getNodeId node) bytes

  Tree.WriteSuccess rt' ns' <- iterateOpUntilSuccess Tree.insert rt k v
  return $ Tree.WriteSuccess rt' (rt:ns ++ ns')

updateMeta :: Binary t => Show t => Tree t -> String -> t -> IO (Tree.WriteStatus t)
updateMeta tr k v = updateNode tr (Tree.Meta k) v

updateData :: Binary t => Show t => Tree t -> String -> t -> IO (Tree.WriteStatus t)
updateData tr k v = updateNode tr (Tree.Data k) v

removeNode :: Tree TreeValue -> Tree.Key -> IO (Tree.WriteStatus TreeValue)
removeNode tr k = do
  iterateOpUntilSuccess (\tr k v ns ids -> Tree.remove tr k ns ids) tr k (DirMetadata 0)

removeMeta :: Tree TreeValue -> String -> IO (Tree.WriteStatus TreeValue)
removeMeta tr k = removeNode tr (Tree.Meta k)

removeData :: Tree TreeValue -> String -> IO (Tree.WriteStatus TreeValue)
removeData tr k = removeNode tr (Tree.Data k)


lookupNode :: Binary t => Show t => Tree t -> Tree.Key -> IO (Tree.ReadStatus t)
lookupNode tr k =
  let aux :: Binary t => Show t => Tree t -> [Tree t] -> IO (Tree.ReadStatus t)
      aux tr nodes = do
        attempt <- return $ Tree.lookup tr k nodes
        case attempt of
          Tree.ReadSuccess _ -> return attempt
          Tree.ReadDoesNotExist -> return attempt
          Tree.ReadError str -> error $ "Error reading node: " ++ str
          Tree.ReadNeedNode id -> do
            bytes <- Store.readNode "/tmp/becfs" id
            tr' <- return $ (runGet Tree.deserialize bytes)
            aux tr $ nodes ++ [tr']
  in
  aux tr []

lookupMeta :: Binary t => Show t => Tree t -> String -> IO (Tree.ReadStatus t)
lookupMeta tr k = lookupNode tr (Tree.Meta k)

lookupData :: Binary t => Show t => Tree t -> String -> IO (Tree.ReadStatus t)
lookupData tr k = lookupNode tr (Tree.Data k)

childEntries tr k =
  let aux :: Binary t => Show t => Tree t -> IO (Tree.ReadStatus [(Tree.Key, t)])
      aux tr = do
        attempt <- return $ Tree.childEntries tr k
        case  attempt of
          Tree.ReadSuccess _ -> return attempt
          Tree.ReadDoesNotExist -> return attempt
          Tree.ReadError str -> error $ "Error reading node: " ++ str
          Tree.ReadNeedNode id -> error $ "ChildEntries shouldn't Need a Node"
          Tree.ReadRepeatOn nodes -> do
               listOfLists <- flip mapM nodes $ \n -> do
                    childTr <- fetchNode n
                    childRes <- aux childTr
                    case childRes of
                      Tree.ReadSuccess l -> return l
                      _ -> error $ "Error reading childEntries: expected Success, got " ++ show childRes
               lists <- return $ concat listOfLists
               return $ Tree.ReadSuccess lists
  in
  aux tr


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
                         fuseCreateSymbolicLink = fsCreateSymlink,
                         fuseReadSymbolicLink = fsReadSymlink,
                         fuseRemoveLink = fsRemoveLink,
                         fuseRemoveDirectory = fsRemoveDirectory,
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
                         fuseSetOwnerAndGroup = fsSetOwnerAndGroup,
                         fuseAccess = fsAccess,
                         fuseInit = fsInit
                       }

fetchRootNode :: Show t => Binary t => IO (Tree t)
fetchRootNode = do
  root_bytes <- Store.readRoot "/tmp/becfs"
  return $ (runGet Tree.deserialize root_bytes)

fetchIORootNode :: Show t => Binary t => IO (IOTree t)
fetchIORootNode = do
  root_bytes <- Store.readRoot "/tmp/becfs"
  return $ (runGet IOTree.deserialize root_bytes)

fetchNode :: Show t => Binary t => Int -> IO (Tree t)
fetchNode n = do
  bytes <- Store.readNode "/tmp/becfs" n
  return $ (runGet Tree.deserialize bytes)

saveNodeUpdates :: Binary t => Show t => Tree.WriteStatus t -> IO ()
saveNodeUpdates (Tree.WriteSuccess root nodes) = do
  root_bytes <- return $ runPut $ Tree.serialize root
  Store.writeRoot "/tmp/becfs" (Tree.getNodeId root) root_bytes
  flip mapM_ nodes $ \node -> do
      bytes <- return $ runPut $ Tree.serialize node
      writeErr $ "writing node " ++ (show $ Tree.getNodeId node)
      Store.writeNode "/tmp/becfs" (Tree.getNodeId node) bytes

saveNodeUpdates status = error $ "Didn't get writesuccess: " ++ show status


updateTreeRetIO :: Show t => Binary t => (Tree t -> IO ((Tree.WriteStatus t), r)) -> IO r
updateTreeRetIO op = do
  takeLock
  tree <- fetchRootNode
  (status, res) <- op tree
  saveNodeUpdates status
  releaseLock
  return res

updateTreeIO :: Show a => Binary a => (Tree a -> IO (Tree.WriteStatus a)) -> IO ()
updateTreeIO op =
  updateTreeRetIO (\tr -> op tr >>= \tr' -> return (tr', ()))

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

loadRoot :: String -> IO (Tree TreeValue)
loadRoot path = do
  root_bytes <- Store.readRoot path
  return $ (runGet Tree.deserialize root_bytes)

fileContents :: FilePath -> IO (Maybe B.ByteString)
fileContents path = do
  return $ Just B.empty{-
  tree <- getTree treefile
  return $ Tree.lookupData tree path-}

{- TODO: Delete the data, too -}
removeFile :: FilePath -> IO ()
removeFile path = do
  updateTreeIO (\t -> removeMeta t path :: IO (Tree.WriteStatus TreeValue))
  updateTreeIO (\t -> removeData t path :: IO (Tree.WriteStatus TreeValue))

removeDir :: FilePath -> IO ()
removeDir path = do
  updateTreeIO (\t -> removeMeta t path :: IO (Tree.WriteStatus TreeValue))

createFileMeta :: FilePath -> IO ()
createFileMeta path =  do
  metadata <- emptyFileMetadata
  --writeErr $ "  Created metadata for " ++ show path
  putStrLn "TODO"
  --updateTreeIO $ \t -> insertMeta t path metadata

ensureFileExists :: FilePath -> IO ()
ensureFileExists path = do
  exists <- fileExists path
  if not exists then
    createFileMeta path
  else
    return ()

writeToFile :: FilePath -> B.ByteString -> FileOffset -> IO ()
writeToFile path contents offset' = do
  offset <- return $ fromIntegral offset'
  baseFileMetadata <- emptyFileMetadata
  ensureFileExists path
  updateTreeIO (\t ->
    lookupMeta t path >>= \oldMeta ->
    let metadata = case oldMeta of
          Tree.ReadSuccess m -> m
          Tree.ReadDoesNotExist -> baseFileMetadata
          _ -> error $ "Got bad metadata writing to file " ++ show path
        length' = max (B.length contents + offset) (filesize metadata)
    in
    updateMeta t path (metadata { filesize = length' }))
  tree <- loadRoot "/tmp/becfs"
  updateTreeIO (\t ->
    lookupData t path >>= \oldData ->
    let oldcontents = case oldData of
          Tree.ReadSuccess (Data d) -> d
          Tree.ReadDoesNotExist -> B.empty
          _ -> error $ "Got bad data writing to file " ++ show path
        prefix = B.take offset oldcontents
        suffix = B.drop (offset + B.length contents) oldcontents
        prefix' = if B.length prefix == offset
                  then prefix
                  else B.append prefix $ By.replicate (offset - B.length prefix) (fromIntegral 0)
    in
    updateData t path (Data $ B.concat [prefix', contents, suffix]))

moveFile :: FilePath -> FilePath -> IO ()
moveFile from to = do
  baseFileMetadata <- emptyFileMetadata
  putStrLn "TODO"
{-updateTree (\t ->
      let metadata = baseFileMetadata{-case Tree.lookupMeta t from of
            Just m -> m
            Nothing -> baseFileMetadata-}
      in
      case Tree.removeMeta t from [] [] of
        Tree.WriteSuccess t' _ -> Tree.insertMeta t' to metadata [] []
        _ -> error "Didn't get WriteSuccess!")
  updateTree (\t ->
      let contents = B.pack "Unimplemented"{-case Tree.lookupData t from of
            Just b -> b
            Nothing -> Data $ B.pack "Lost contents!"-}
      in
      case Tree.removeData t from [] [] of
        Tree.WriteSuccess t' _ -> Tree.insertData t' to contents [] []
        _ -> error "Didn't get WriteSuccess!")-}

createSymlink :: FilePath -> FilePath -> IO Errno
createSymlink from to = do
  -- TODO: Error checking
  now <- getNow
{-updateTree (\t ->
    Tree.insertMeta t from (SymlinkMetadata { target = to, mtime = now }) [] [])-}
  return eOK

setPerms :: FilePath -> FileMode -> IO Errno
setPerms path perms = do
  updateTreeRetIO (\t -> do
      meta <- lookupMeta t path
      case meta of
        Tree.ReadDoesNotExist -> return (Tree.WriteSuccess t [], eNOENT)
        Tree.ReadSuccess m -> do
            update <- updateMeta t path (m { perms = perms })
            return (update, eOK)
        other -> error $ "Bad setperms result: " ++ show other
      )

setOwnerGroup :: FilePath -> UserID -> GroupID -> IO Errno
setOwnerGroup path uid gid = do
  updateTreeRetIO (\t -> do
      meta <- lookupMeta t path
      case meta of
        Tree.ReadDoesNotExist -> return (Tree.WriteSuccess t [], eNOENT)
        Tree.ReadSuccess m -> do
            update <- updateMeta t path (m { owner = uid, group = gid })
            return (update, eOK)
        other -> error $ "Bad setperms result: " ++ show other
      )


fileExists :: FilePath -> IO Bool
fileExists path = do
  return False{-
  tree <- (getTree treefile :: IO (Tree TreeValue))
  case Tree.lookupMeta tree path of
    Just _ -> return True
    _ -> return False-}

isMeta :: Tree.Key -> Bool
isMeta (Tree.Meta{}) = True
isMeta _ = False

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
    statFileMode = perms metadata,
    statLinkCount = 1,
    statFileOwner = owner metadata,
    statFileGroup = group metadata,
    statSpecialDeviceID = 0,
    statFileSize = fromIntegral $ filesize metadata,
    statBlocks = 1,
    statAccessTime = 0,
    statModificationTime = fromIntegral $ mtime metadata,
    statStatusChangeTime = 0
  }

symlinkStat metadata ctx = FileStat {
    statEntryType = SymbolicLink,
    statFileMode = foldr1 unionFileModes [ ownerReadMode, ownerExecuteMode,
                                           groupReadMode, groupExecuteMode,
                                           otherReadMode, otherExecuteMode ],
    statLinkCount = 1,
    statFileOwner = fuseCtxUserID ctx,
    statFileGroup = fuseCtxGroupID ctx,
    statSpecialDeviceID = 0,
    statFileSize = 4096,
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
  --writeErr $ "  statting " ++ show path ++ ": loaded tree " ++ show tree
  root <- fetchRootNode
  meta <- if path == "/.tree" then do
            result <- showBtree "" :: IO String
            Tree.ReadSuccess <$> dummyFileMetadata (length result)
          else
            lookupMeta root path
  case meta of
    Tree.ReadSuccess metadata -> do
      writeErr $ "  statting " ++ show path ++ " => " ++ show metadata
      ctx <- getFuseContext
      case metadata of
        FileMetadata{} -> return $ Right $ fileStat metadata ctx
        DirMetadata{} -> return $ Right $ dirStat metadata ctx
        SymlinkMetadata{} -> return $ Right $ symlinkStat metadata ctx
    Tree.ReadDoesNotExist -> return $ Left eNOENT
    other -> error $ "Bad fsGetFileStat:" ++ show other

fsOpenDir _ = return eOK

fsReadDir :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
fsReadDir dir' = do
  dir <- return $ if last dir' == '/' then dir' else dir' ++ "/"
  ctx <- getFuseContext
  metadata <- emptyDirMetadata
  tree <- fetchRootNode :: IO (Tree TreeValue)
  Tree.ReadSuccess entries <- childEntries tree dir
  writeErr $ "Reading dir " ++ show dir ++ " => " ++ show entries
  baseFileMetadata <- emptyFileMetadata
  val <- return $ Right $ [(".", dirStat metadata ctx),
                    ("..", dirStat metadata ctx)] ++
                   map (\(Tree.Meta path, meta) ->
                       let path' = reverse $ takeWhile (/= '/') $ reverse path in
                       case meta of
                         FileMetadata{} -> (path', fileStat meta ctx)
                         DirMetadata{} -> (path', dirStat meta ctx)
                         SymlinkMetadata{} -> (path', symlinkStat meta ctx)
                       ) entries
  writeErr $ "  read dir " ++ show dir ++ " => " ++ (case val of Right dirs -> show dirs; Left _ -> "ERROR")
  return val

fsOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno ())
fsOpen path mode flags = do
  writeErr $ "Opening path " ++ show path
  tree <- fetchRootNode :: IO (Tree TreeValue)
  meta <- if path == "/.tree" then do
            result <- showBtree "" :: IO String
            Tree.ReadSuccess <$> dummyFileMetadata (length result) :: IO (Tree.ReadStatus TreeValue)
          else
            lookupMeta tree path
  case meta of
    Tree.ReadSuccess _ ->
      case mode of
        ReadOnly -> return $ Right ()
        WriteOnly -> return $ Right ()
        _ -> return $ Left eACCES
    Tree.ReadDoesNotExist -> return $ Left eNOENT
    status -> error $ "Bad return from fsOpen: " ++ show status

fsRead :: FilePath -> () -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
fsRead path () byteCount offset = do
  writeErr $ "Reading path " ++ show path
  tree <- fetchRootNode :: IO (Tree TreeValue)
  if path == "/.tree" then do
    writeErr "Reading tree special case"
    Right <$> B.pack <$> showBtree ""
  else do
    result <- lookupData tree path
    case result of
        Tree.ReadSuccess (Data c) -> return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) c
        Tree.ReadDoesNotExist -> return $ Left eNOENT
        _ -> return $ Left eNOSYS

fsWrite :: FilePath -> () -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
fsWrite path () bytes offset = do
  writeErr $ "Writing path " ++ show path ++ " at offset " ++ show offset
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
  updateTreeIO (\t -> insertMeta t path metadata)
  return eOK

fsCreateSymlink :: FilePath -> FilePath -> IO Errno
fsCreateSymlink to from =  do
  writeErr $ "Creating symlink " ++ show from ++ " => " ++ show to
  createSymlink from to

fsReadSymlink :: FilePath -> IO (Either Errno FilePath)
fsReadSymlink path =  do
  return $ Left eNOENT
{-tree <- getTree treefile
  case Tree.lookupMeta tree path of
    Just (SymlinkMetadata {target = t}) -> return $ Right t
    _ -> return $ Left eNOENT -}

fsRemoveLink :: FilePath -> IO Errno
fsRemoveLink path = do
  writeErr $ "Unlinking " ++ show path
  removeFile path
  return eOK

fsRemoveDirectory :: FilePath -> IO Errno
fsRemoveDirectory path = do
  writeErr $ "Unlinking dir " ++ show path
  removeDir path
  return eOK

fsRename :: FilePath -> FilePath -> IO Errno
fsRename from to = do
  writeErr $ "Renaming " ++ show from ++ " => " ++ show to
  moveFile from to
  return eOK

fsSetFileMode :: FilePath -> FileMode -> IO Errno
fsSetFileMode path perms = do
  writeErr $ "Setting perms on " ++ show path ++ " => " ++ show perms
  setPerms path perms

fsSetOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO Errno
fsSetOwnerAndGroup path uid gid = do
  writeErr $ "Setting owner/group on " ++ show path ++ " => " ++ show uid ++ "/" ++ show gid
  setOwnerGroup path uid gid

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

fsAccess :: FilePath -> Int -> IO Errno
fsAccess str uid = return eOK

fsInit :: IO ()
fsInit = do
  Store.writeRoot "/tmp/becfs" 1 $ runPut $ Tree.serialize (Tree.empty :: Tree TreeValue)
