-- file: pw.hs

import System.Environment
import System.Exit
import ApplicativeParsec
import PreProcessWmlIO
import ParseWml
import Utf8ReadWriteFile

main = do
    args <- getArgs
    doIt args

doIt [i] = pw i o
    where o = "pw.parse"
doIt [i,o] = pw i o
doIt _ = do
    usage
    exitFailure

usage = do
    p <- getProgName
    print ("Usage: " ++ p ++ " input [outfile]")

pw x y = do 
    p <- preProcessWmlFile x; 
    let r = parseWml p 
    writeFileUtf8 y (show r)

    
