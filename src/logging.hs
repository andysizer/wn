-- file logging.hs

module Logging
(
  Logging.log
)
    where

import System.IO

log logName msg = do
    putStr $ logName ++ ":\t" ++ msg