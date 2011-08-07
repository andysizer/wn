-- file: preprocessWmlIO.hs

module PreProcessWmlIO
(
  PreProcessWmlIO.preProcessWmlFile
) 
    where

import Data.List as L
import System.Directory
import System.FilePath

import FileSystem
import Utf8ReadWriteFile
import Logging as Wesnoth
import PreProcessWml as PP
    
preProcessWmlFile f = driver $ initState f

driver :: PreProcessorState -> IO String
driver (PreProcessorState Nothing [] _ _ _ _ _ _ _) = return ""
driver (PreProcessorState (Just path) [] defines dom sdepth pd pb is h) = do
    Wesnoth.log "PP" ("preprocessing* " ++ path ++ "\n")
    files <- expandPath "" path
    Wesnoth.log "PP" (path ++ " expanded to* " ++ (show files) ++ "\n")
    driver $ PreProcessorState Nothing (L.map File files) defines dom sdepth pd pb is h
driver (PreProcessorState (Just path) work@((Cont _ file): _) defines dom sdepth pd pb is h) = do
    Wesnoth.log "PP" ("including " ++ path ++ "\n")
    files <- expandPath (dropFileName file) path
    Wesnoth.log "PP" (path ++ " expanded to " ++ (show files) ++ "\n")
    driver $ PreProcessorState Nothing (L.map File files ++ work) defines dom sdepth pd pb is h
driver (PreProcessorState Nothing ((Cont cont file): _) defines _ _ _ _ _ _) = do
    Wesnoth.log "PP" ("continuing " ++ file ++ "\n")
    let (pps', result) = cont defines 
    result' <- driver pps'
    return $ result ++ result'
driver (PreProcessorState Nothing ((File file): work) defines dom sdepth pd pb is h) = do
    let sn = getShortWmlPath file
    let pps = PreProcessorState Nothing work defines dom sdepth pd pb is h
    Wesnoth.log "PP" ("preprocessing " ++ sn ++ "\n")
    s <- readFileUtf8 file
    let (pps', result) = PP.preProcessWmlFile pps sn s
    Wesnoth.log "PP" ("result >>>\n" ++ result)
    result' <- driver pps'
    return $ result ++ result'

expandPath _ "" = do
    Wesnoth.log "ERR" "empty path given to expandPath\n"
    return []
expandPath currentDir path = do
    let expandDir = do
        (fs,_,_) <- getFilesInDir path True False EntireFilePath SkipMediaDir DoReorder Nothing
        return fs
    let expandPath' = do
        p <-  getWmlLocation path currentDir
        expandPath currentDir p
    let expandFile = do
        isFile <- doesFileExist path
        if isFile
        then return [path] 
        else expandPath'
    isDir <- isDirectory path
    if isDir
    then expandDir
    else expandFile

----------

tc1 =  "c:\\Users\\andy\\haskell\\wn\\src\\tc1.cfg"