-- file: fileSystem.hs

module FileSystem
(
  getFilesInDir
, FileNameOption(..)
, FileFilterOption(..)
, FileReorderOption(..)
, dataTreeCheckSum
, getWmlLocation
, getShortWmlPath
, getFileSize
, isDirectory
, directoryName
, fileName
) 
    where

import Control.Monad

import System.IO 
import System.Directory
import System.FilePath
import System.Time
import System.Info

import Data.List as L
import Data.Maybe

import GameConfig

import Logging as Wesnoth

data FileNameOption = EntireFilePath | FileNameOnly 
    deriving (Eq, Ord, Show)
data FileFilterOption = NoFilter | SkipMediaDir 
    deriving (Eq, Ord, Show)
data FileReorderOption = DontReorder | DoReorder 
    deriving (Eq, Ord, Show)

data CheckSumResult = CheckSumResult
    {
      numFiles :: Integer
    , sumSize  :: Integer
    , modified :: ClockTime
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

getFilesInDir :: FilePath ->  Bool -> Bool -> FileNameOption -> FileFilterOption -> FileReorderOption -> Maybe CheckSumResult
                 -> IO ([FilePath],[FilePath], Maybe CheckSumResult) 
getFilesInDir [] True dirs nameOption filterOption DoReorder checkSum = 
    tryMainCfg [] mainCfgFilename dirs nameOption filterOption checkSum
getFilesInDir dir True dirs nameOption filterOption DoReorder checkSum
    | isWmlAbsolute dir = do
        let path = normalise $ combine dir mainCfgFilename
        tryMainCfg dir path dirs nameOption filterOption checkSum
    | otherwise = do
        let path = normalise $ combine gameConfigPath (combine dir mainCfgFilename)
        tryMainCfg dir path dirs nameOption filterOption checkSum
getFilesInDir dir files dirs nameOption filterOption reorderOption  checkSum
    | isWmlAbsolute dir = do
        getFilesInDir' dir files dirs nameOption filterOption reorderOption checkSum
    | otherwise = do
        let path = normalise $ combine gameConfigPath dir
        getFilesInDir' path files dirs nameOption filterOption reorderOption checkSum

tryMainCfg dir path dirs nameOption filterOption cs = do
    mainExists <- doesFileExist path
    if mainExists
    then case nameOption of
             EntireFilePath -> returnFilesResult dir "tryMainCfg" ([path],[], cs) nameOption filterOption
             FileNameOnly -> returnFilesResult dir "tryMainCfg" ([mainCfgFilename],[], cs) nameOption filterOption
    else getFilesInDir' dir True dirs nameOption filterOption DoReorder cs

returnFilesResult dir fn r@(files, dirs, cs) n f = do
    Wesnoth.log "FS" (fn ++ " " ++ dir ++ " -> (" ++ show files ++ ", " ++ show dirs ++ ") " ++ show n ++ " " ++ show f ++ "\n")
    return r 

getFilesInDir' path files dirs nameOption filterOption reorderOption  cs = do
    let addFile f (fs,ds,cs) = do
        let fs' = case nameOption of
                      EntireFilePath -> ( (normalise $ combine path f) : fs)
                      _ -> (f : fs)
        cs' <- case cs of
                   Just x -> updateCheckSum (normalise $ combine path f) x
                   _ -> return cs
        return (fs', ds, cs')
    let maybeReorder d a@(fs,ds,cs) = do
        let buildResult d (fs,ds,cs) =  do
            case nameOption of
                EntireFilePath -> return (fs, ((normalise $ combine path d) : ds), cs)
                _ -> return (fs, (d : ds), cs)
        let tryMain d m a@(fs,ds,cs) = do
            mainExists <- doesFileExist path
            if mainExists
            then addFile m a
            else buildResult d a
        case reorderOption of
            DontReorder -> buildResult d a
            _ -> tryMain d (normalise $ combine path (combine d mainCfgFilename)) a
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
           isFile <- doesFileExist $ normalise $ combine path e;
           if isFile
           then addFile e r'
           else addDir e r'
        }
    contents <- getDirectoryContents path
    let acc = return ([],[],cs)
    (fs, ds, cs') <- foldr addEntry acc contents
    returnFilesResult path "getFilesInDir'" (reorderFiles reorderOption fs, sort ds, cs') nameOption filterOption
                                      
reorderFiles DontReorder fs = sort fs
reorderFiles DoReorder fs = i ++ sort fs'' ++ f
    where (i,fs') = L.partition ((==) initialCfgFilename) fs
          (f,fs'') = L.partition ((==) finalCfgFilename) fs'

dataTreeCheckSum cs = do
    let cs' = case cs of
                  Nothing -> Just mkCheckSumResult
                  _ -> cs
    cs'' <- fileTreeCheckSum cs' "data/"
    cs''' <- fileTreeCheckSum cs'' (normalise $ combine userDataPath "data")
    Wesnoth.log "FS" ("calculated data tree checksum: " 
                 ++ (show $ numFiles $ fromJust cs''') ++ " files; "
                 ++ (show $ sumSize $ fromJust cs''') ++ " bytes\n")
    return cs'''

fileTreeCheckSum Nothing p = return Nothing
fileTreeCheckSum cs p = do
    (_,ds,cs') <- getFilesInDir p False True EntireFilePath SkipMediaDir DontReorder cs
    foldM fileTreeCheckSum cs' ds

getWmlLocation :: FilePath -> FilePath -> IO FilePath
getWmlLocation filename currentDir = do
    Wesnoth.log "DBG" ("Looking for '" ++ filename ++ "'.\n");
    getWmlLocation' filename currentDir

getWmlLocation' "" _ = do
    Wesnoth.log "FS" "Invalid filename\n"
    return ""
getWmlLocation' filename@(c:cs) currentDir
    | ".." `isInfixOf` filename = do
        Wesnoth.log "ERR" ("Illegal path '" ++ filename ++ "' (\"..\" not allowed).\n")
        return ""
    | c == '~' = do
        let fn = normalise $ combine userDataPath (combine "data" cs)
        checkExists fn
    | "./" `isPrefixOf` filename = do
        case stripPrefix "./" filename of
            Just f -> checkExists $ normalise $ combine currentDir f
            _ -> do
                 {  Wesnoth.log "ERR" "failed to strip prefix"; -- should never get this
                    return ""
                 }
    | otherwise = do
        let fn = normalise $ combine gameConfigPath (combine "data/" filename)
        checkExists fn

checkExists :: FilePath -> IO FilePath
checkExists fn = do
    Wesnoth.log "FS" ("trying '" ++ fn ++ "'\n")
    fExists <- doesFileExist fn
    if fExists
    then do
         { Wesnoth.log "FS" ("found: '" ++ fn ++ "'\n");
           return fn
         }
    else do 
         { dExists <- doesDirectoryExist fn;
           if dExists
           then do
                { Wesnoth.log "FS" ("found: '" ++ fn ++ "'\n");
                  return fn
                }
           else do
                { Wesnoth.log "FS" "not found\n";
                  return ""
                }
         }

getShortWmlPath :: FilePath -> FilePath
getShortWmlPath fn = sfn
    where sfn = case shortenPath userDataPath fn "~" of
                    Just f -> f
                    _ -> case shortenPath gameConfigPath fn "" of
                             Just f -> f
                             _ -> fn

shortenPath p fn pre =
    case stripPrefix (normalise $ combine p "data/") (normalise fn) of
        Just f -> Just $ pre ++ f
        e -> e

exeext = case os of
             "mingw32" -> "exe"
             _ -> ""

getProgramInvocation p = normalise $ combine wesnothProgDir (addExtension p exeext)

directoryName = takeDirectory

isWinWmlAbsolute "" = False
isWinWmlAbsolute d@(c:cs)
    | c == '\\' = True
    | c == '/' = True
    | ":\\" `isPrefixOf` cs = True
    | ":/" `isPrefixOf` cs = True
    | otherwise = False

isWmlAbsolute = case os of
             "mingw32" -> isWinWmlAbsolute
             _ -> isAbsolute

isDirectory "" = return False
isDirectory d
    | isWmlAbsolute d = do
        doesDirectoryExist d
    | otherwise = do
        doesDirectoryExist $ normalise $ combine gameConfigPath d

fileName = takeFileName

