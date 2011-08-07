-- file logging.hs

module Logging
(
  Logging.log
)
    where

import System.IO

-- supported logs: "FS", "DBG", "ERR", "PP"

--enabledLogs = []
enabledLogs = ["FS", "DBG", "ERR", "PP"]

log logName msg = do
    if logName `elem` enabledLogs
    then putStr $ logName ++ ":\t" ++ msg
    else return ()