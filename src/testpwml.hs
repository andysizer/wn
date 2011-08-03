-- file: testpwml.hs

import System.Environment
import System.Exit
import ApplicativeParsec
import PreProcessWmlIO
import ParseWml
import Utf8ReadWriteFile

main = do
    args <- getArgs
    doIt args

doIt [i] = pp i o
    where o = "pp.txt"
doIt [i,o] = pp i o
doIt _ = do
    usage
    exitFailure

usage = do
    p <- getProgName
    print ("Usage: " ++ p ++ " input [outfile]")

pp x y = do 
    p <- preProcessWmlFile x
    case parseWml p of
        (Right r) -> writeFileUtf8 y (show r)
        e -> print e
       
