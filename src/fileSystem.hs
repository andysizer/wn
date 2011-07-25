-- file: fileSystem.hs

module FileSystem
(
  getFilesInDir
, FileNameOption(..)
, FileFilterOption(..)
, FileReorderOption(..)
) 
    where

import System.IO 
import System.Directory
import System.FilePath
import System.Time

import Data.List as L

import GameConfig

import Logging as Lg

data FileNameOption = EntireFilePath | FileNameOnly
data FileFilterOption = NoFilter | SkipMediaDir
data FileReorderOption = DontReorder | DoReorder

data CheckSumResult = CheckSumResult
    {
      numFiles :: Integer
    , sumSize  :: Integer
    ,  modified :: ClockTime
    }
        deriving (Eq, Ord, Show)

mkCheckSumResult = CheckSumResult 0 0 (TOD 0 0)

updateCheckSum p (CheckSumResult n s t) = do
    t' <- getModificationTime p
    s' <- getFileSize p
    return $ Just $ CheckSumResult (n+1) (s + s') (max t t')

getFileSize p = do
    h <- openFile p ReadMode
    s <- hFileSize h
    hClose h  
    return s

dirSep = pathSeparator

mainCfgFilename = "_main.cfg"
finalCfgFilename = "_final.cfg"
initialCfgFilename = "_initial.cfg"

getFilesInDir :: FilePath ->  Bool -> Bool -> FileNameOption -> FileFilterOption -> FileReorderOption -> Bool 
                 -> IO ([FilePath],[FilePath], Maybe CheckSumResult) 
getFilesInDir dir True dirs nameOption filterOption DoReorder needCheckSum = 
    tryMainCfg dir path dirs nameOption filterOption needCheckSum
    where path = case dir of
                     [] -> mainCfgFilename
                     (dirSep : _ ) -> (combine dir mainCfgFilename)
                     _ -> (combine gameConfigPath (combine dir mainCfgFilename))
getFilesInDir dir files dirs nameOption filterOption reorderOption  needCheckSum =
    getFilesInDir' path files dirs nameOption filterOption reorderOption needCheckSum
    where path = case dir of
                     (dirSep : _ ) -> dir
                     _ -> combine gameConfigPath dir


tryMainCfg dir path dirs nameOption filterOption needCheckSum = do
    mainExists <- doesFileExist path
    let cs = if needCheckSum then Just mkCheckSumResult else Nothing
    if mainExists
    then case nameOption of
             EntireFilePath -> return ([path],[], cs)
             FileNameOnly -> return ([mainCfgFilename],[], cs)
    else getFilesInDir' dir True dirs nameOption filterOption DoReorder needCheckSum

getFilesInDir' path files dirs nameOption filterOption reorderOption  needCheckSum = do
    let addFile f (fs,ds,cs) = do
        let fs' = case nameOption of
                      EntireFilePath -> ( combine path f : fs)
                      _ -> (f : fs)
        cs' <- case cs of
                   Just x -> if needCheckSum then updateCheckSum (combine path f) x else return cs
                   _ -> return cs
        return (fs', ds, cs')
    let maybeReorder d a@(fs,ds,cs) = do
        let buildResult d (fs,ds,cs) =  do
            case nameOption of
                EntireFilePath -> return (fs, (combine path d : ds), cs)
                _ -> return (fs, (d : ds), cs)
        let tryMain d m a@(fs,ds,cs) = do
            mainExists <- doesFileExist path
            if mainExists
            then addFile m a
            else buildResult d a
        case reorderOption of
            DontReorder -> buildResult d a
            _ -> tryMain d (combine path (combine d mainCfgFilename)) a
    let addDir d a@(fs,ds,cs) = do
        case d of
            "." -> return a
            ".." -> return a
            _ -> case filterOption of
                     SkipMediaDir -> 
                         case d of
                             "images" -> return a
                             "sounds" -> return a
                             _ -> maybeReorder d a
                     _ -> maybeReorder d a
    let addEntry e r = do
        {  r' <- r;
           isFile <- doesFileExist $ combine path e;
           if isFile
           then addFile e r'
           else addDir e r'
        }
    contents <- getDirectoryContents path
    let cs = if needCheckSum then Just mkCheckSumResult else Nothing
    let acc = return ([],[],cs)
    (fs, ds, cs) <- foldr addEntry acc contents
    return $ (reorderFiles reorderOption fs, sort ds, cs)
                                      
reorderFiles DontReorder fs = sort fs
reorderFiles DoReorder fs = i ++ sort fs'' ++ f
    where (i,fs') = L.partition ((==) initialCfgFilename) fs
          (f,fs'') = L.partition ((==) finalCfgFilename) fs'

getWmlLocation :: FilePath -> FilePath -> IO FilePath
getWmlLocation filename currentDir = do
    Lg.log "DBG" ("Looking for '" ++ filename ++ "'.\n");
    getWmlLocation' filename currentDir

getWmlLocation' "" _ = do
    Lg.log "FS" "Invalid filename\n"
    return ""
getWmlLocation' filename@(c:cs) currentDir
    | isInfixOf ".." filename = do
        Lg.log "ERR" ("Illegal path '" ++ filename ++ "' (\"..\" not allowed).\n")
        return ""
    | c == '~' = do
        let fn = combine userDataPath (combine "data" cs)
        Lg.log "FS" ("trying '" ++ fn ++ "'\n")
        fExists <- doesFileExist fn
        if fExists
        then return fn
        else do 
             { dExists <- doesDirectoryExist fn;
               if dExists
               then return fn
               else do
                    { Lg.log "FS" "not found\n";
                      return ""
                    }
             }
    | otherwise = do
        return ""