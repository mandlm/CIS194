module LogAnalysis where

import Text.Read ( readMaybe )

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)
  
type Timestamp = Int

data LogMessage = LogMessage MessageType Timestamp String
  deriving (Show, Eq)
  
data MaybeLogMessage = ValidLM LogMessage
                     | InvalidLM String
  deriving (Show, Eq)
  
data MaybeInt = ValidInt Int
              | InvalidInt
  deriving (Show, Eq)
  
readInt :: String -> MaybeInt
readInt s 
  | Just i <- readMaybe s = ValidInt i
  | otherwise = InvalidInt
  
-- parseMessage :: String -> MaybeLogMessage
