-- file: utf8ReadWriteFile.hs

module Utf8ReadWriteFile
(
  readFileUtf8
, writeFileUtf8
) 
    where

import System.IO 

readFileUtf8 f = do
   h <- openFile f ReadMode
   hSetEncoding h utf8
   s <- hGetContents h
   return s

writeFileUtf8 f s = do
   h <- openFile f WriteMode
   hSetEncoding h utf8
   hPutStr h s
   hClose h

