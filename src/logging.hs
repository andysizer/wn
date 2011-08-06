-- file logging.hs

module Logging
(
  Logging.log
)
    where

import System.IO

enabledLogs = []

log logName msg = do
    if logName `elem` enabledLogs
    then putStr $ logName ++ ":\t" ++ msg
    else return ()