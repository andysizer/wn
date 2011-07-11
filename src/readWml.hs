
module WML where

import PreProcessWml (preProcessWmlFile)

import Prelude

data Node = Node

readWml:: FilePath -> IO [Node]
readWml f =
    do src <- readFile f
       pre <- preProcessWmlFile f src
       nodes <- parseWml pre
       return nodes

parseWml s = undefined