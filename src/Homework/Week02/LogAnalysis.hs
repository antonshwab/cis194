module Homework.Week02.LogAnalysis (
  build,
  inOrder,
  insert,
  parse,
  parseMessage,
  whatWentWrong,
  LogMessage(..),
  MessageTree(..),
  MessageType(..),
  TimeStamp
) where

import Homework.Week02.Log

-- #1a
parseMessage :: String -> LogMessage
parseMessage line =
  case words line of
    ("I":timestamp:rest)          -> LogMessage Info (read timestamp) (unwords rest)
    ("W":timestamp:rest)          -> LogMessage Warning (read timestamp) (unwords rest)
    ("E":severity:timestamp:rest) -> LogMessage (Error (read severity)) (read timestamp) (unwords rest)
    _                             -> Unknown line

-- #1b
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree                                    = tree
insert logMessage Leaf                                     = Node Leaf logMessage Leaf
insert logMessage@(LogMessage _ timestamp _)
       (Node left currentLogMessage@(LogMessage _ currentTimestamp _) right) = if timestamp < currentTimestamp
                                                             then Node (insert logMessage left) currentLogMessage right
                                                             else Node left currentLogMessage (insert logMessage right)

-- #3
build :: [LogMessage] -> MessageTree
-- build logMessages = go logMessages Leaf
--   where
--     go [] tree = tree
--     go (logMessage:rest) tree = go rest (insert logMessage tree)
build = foldl (flip insert) Leaf

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMessage right) = inOrder left ++ [logMessage] ++ inOrder right

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map mapGo . filter filterGo . inOrder . build
  where
    mapGo (LogMessage _ _ message) = message
    filterGo (LogMessage (Error severity) _ _) = severity > 50
    filterGo _ = False
