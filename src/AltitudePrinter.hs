module AltitudePrinter
    ( prettyPrint
    ) where

import AltitudePrinter.Data
import Data.Maybe
import Control.Monad.State.Lazy
import Data.List
import Data.Ord

data ServerState =
  ServerState
    { getPlayers :: [Player] }

data LogElement = LogElement
  { logTime :: Int
  , logHead :: String
  , logContents :: String }

prettyPrint :: String -> String
prettyPrint serverLog =
  let
    events = eventsFromLog serverLog
    eventLog = logEvents events
  in printLog eventLog

{-------------------------------------------------------------------------------
* Forming Logs
-------------------------------------------------------------------------------}

logEvent :: Event -> State ServerState [LogElement]
logEvent (Event time action) = do
  players <- getPlayers <$> get
  case action of
    ChatEvent pid chatStr ->
      case find ((== pid) . getPlayerID) players of
        Nothing -> return [LogElement time "somebody" chatStr]
        Just player -> return [LogElement time (getNick player) chatStr]
    JoinEvent player -> do
      put (ServerState (player:players))
      return . (:[]) . LogElement time "" . concat $
        [ "\"", getNick player
        , "\" (" , getVaporID player
        , ") joins the game" ]
    LeaveEvent player -> do
      put (ServerState (filter ((/= getPlayerID player) . getPlayerID) players))
      return . (:[]) . LogElement time "" . concat $
        [ "\"", getNick player
        , "\" (", getVaporID player
        , ") leaves the game" ]
    _ -> return []

logEvents :: [Event] -> [LogElement]
logEvents events =
  let
    statePrintEvents :: State ServerState [LogElement]
    statePrintEvents = fmap concat . mapM logEvent $ events
  in
    evalState statePrintEvents $ ServerState []

{-------------------------------------------------------------------------------
* Printing Logs
-------------------------------------------------------------------------------}

printLog :: [LogElement] -> String
printLog elements =
  let
    longestTime = length . show $ logTime $ last elements
    longestHead = length . maximumBy (comparing length) $
      map logHead elements
    fillToLength i str = take i $ str ++ repeat ' '
    printElement (LogElement time header contents) =
      concat
        [ fillToLength longestTime (show time)
        , " | "
        , fillToLength longestHead header
        , " "
        , contents ]
  in
    unlines . map printElement $ elements
