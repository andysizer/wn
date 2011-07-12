
module WML where

import PreProcessWml (preProcessWmlFile)

import Prelude

data Node = Node

readWml:: FilePath -> IO [Node]
readWml f =
    do pre <- preProcessWmlFile f
       nodes <- parseWml pre
       return nodes

parseWml s = undefined