-- file: fileSystem.hs

module FileSystem
(
  preProcessWmlFile
, preProcessWml
, pp
, initState
) 
    where

import System.IO 
import System.Directory
import System.FilePath

data FileNameOption = EntireFilePath | FileNameOnly
data FileFilterOption = NoFilter | SkipMediaDir
data FileReorderOption = DontReorder | DoReorder

mainCfgFilename = "_main.cfg"
finalCfgFilename = "_final.cfg"
initialCfgFilename = "_initial.cfg"

getFilesInDir :: FilePath ->  Bool -> Bool -> FileNameOption -> FileFilterOption -> FileReorderOption -> Bool 
                 -> IO ([FilePath],[FilePath], Maybe CheckSumResult) 
getFilesInDir dir@[] True dirs nameOption filterOption DoReorder needCheckSum = 
    tryMainCfg dir mainCfgFilename dirs nameOption filterOption needCheckSum
getFilesInDir dir@(dirSep : _) True dirs nameOption filterOption DoReorder needCheckSum =
    tryMainCfg dir (combine dir mainCfgFilename) dirs nameOption filterOption needCheckSum
getFilesInDir dir True dirs nameOption filterOption DoReorder needCheckSum =
    tryMainCfg dir (combine gameConfigPath (combine dir mainCfgFilename)) dirs nameOption filterOption needCheckSum
getFilesInDir dir@(dirSep : _) files dirs nameOption filterOption reorderOption needCheckSum = 
    getFilesInDir' dir files dirs nameOption filterOption reorderOption needCheckSum
getFilesInDir dir files dirs nameOption filterOption reorderOption  needCheckSum =
    getFilesInDir' path files dirs nameOption filterOption reorderOption needCheckSum
    where path = combine gameConfigPath dir

getFilesInDir' path files dirs nameOption filterOption reorderOption  needCheckSum = do
    contents <- getDirectoryContents path
    let cs = if needCheckSum then Just mkCheckSumResult else Nothing
    acc <- return ([],[],cs)
    let addEntry e r = do
       isFile <- doesFileExist $ combine path e
       if fExist
       then addFile e r
       else addDir e r
    let addFile f (fs,ds,cs) = do
        let fs' = case nameOption of
                      EntireFilePath -> ( combine path f : fs)
                      _ -> (f : fs)
        let cs' = if needCheckSum then updateCheckSum (combine path f) cs else cs
        return (fs', ds, cs')
    let addDir d a@(fs,ds,cs) = do
        case d of
            "." -> return a
            ".." -> return a
            _ -> case filterOption of
                     SkipMediaDir -> 
                         case d of
                             "images" -> return a
                             "sounds" -> return a
                             _ -> case reorderOption of
                                      DontReorder ->
                                          case nameOption of
                                              EntireFilePath -> return (fs, (combine path d : ds), cs)
                                              _ -> return (fs, (d : ds), cs)
                                      _ -> xxxx
                     _ -> yyyy
    return $ foldr addEntry acc contents
                                      
        
    


tryMainCfg dir path dirs nameOption filterOption = do
    mainExists <- doesFileExist path
    if mainExists
    then case nameOption of
             EntireFilePath -> return ([path],[])
             FileNameOnly -> return ([mainCfgFilename],[])
    else getFilesInDir' dir True dirs nameOption filterOption DoReorder
