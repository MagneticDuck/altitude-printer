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
    { getLastTime :: Int
    , getPlayers :: [Player] }

data LogElement = LogElement
  { logTime :: Int
  , logHead :: Maybe String
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

timeNoticeValue :: Int
timeNoticeValue = 1000 * 60 * 30 -- 30 minutes

logEvent :: Event -> State ServerState [LogElement]
logEvent (Event time action) = do
  players <- getPlayers <$> get
  lastTime <- getLastTime <$> get
  let timeNotice =
        [LogElement time Nothing ("---- " ++ printTime (time - lastTime) ++ " passes") | (time - lastTime) > timeNoticeValue]
  case action of
    ChatEvent pid chatStr -> do
      put (ServerState time players)
      case find ((== pid) . getPlayerID) players of
        Nothing -> return . (timeNotice ++) $
          [LogElement time (Just "") chatStr]
        Just player -> return . (timeNotice ++) $
          [LogElement time (Just $ getNick player) chatStr]
    JoinEvent player -> do
      put (ServerState time (player:players))
      return . (timeNotice ++) . (:[]) . LogElement time Nothing . concat $
        [ "---> "
        , "\"", getNick player
        , "\" (" , getVaporID player
        , ")" ]
    LeaveEvent player reason -> do
      put (ServerState time (filter ((/= getPlayerID player) . getPlayerID) players))
      return . (timeNotice ++) . (:[]) . LogElement time Nothing . concat $
        [ "<--- "
        , "\"", getNick player
        , "\" (", getVaporID player
        , "): ", reason ]
    MapEvent mapName -> do
      put (ServerState time players)
      return . (timeNotice ++) . (:[]) . LogElement time Nothing . concat $
        [ "---- switched to map " ++ mapName]
    _ -> return []

logEvents :: [Event] -> [LogElement]
logEvents events =
  let
    statePrintEvents :: State ServerState [LogElement]
    statePrintEvents = fmap concat . mapM logEvent $ events
  in
    evalState statePrintEvents $ ServerState 0 []

{-------------------------------------------------------------------------------
* Printing Logs
-------------------------------------------------------------------------------}

printTime :: Int -> String
printTime time =
  let
    secondTime = 1000
    minuteTime = 60 * secondTime
    hourTime = 60 * minuteTime
    dayTime = 24 * hourTime

    days = time `div` dayTime
    hours = (time - dayTime * days) `div` hourTime
    minutes = (time - days * dayTime - hours * hourTime) `div` minuteTime
    seconds = (time - days * dayTime - hours * hourTime - minutes * minuteTime) `div` secondTime
  in
    show days ++ "d:" ++ show hours ++ "h:" ++ show minutes ++ "m:" ++ show seconds ++ "s"

printLog :: [LogElement] -> String
printLog elements =
  let
    longestTime =
      length . printTime . maximumBy (comparing (length . printTime)) $
        map logTime elements
    longestHead = length . maximumBy (comparing length) $
      mapMaybe logHead elements
    fillToLength i str = take i $ str ++ repeat ' '
    printElement (LogElement time mheader contents) =
      case mheader of
        Nothing ->
          concat
            [ fillToLength longestTime (printTime time)
            , " || ", contents, " ||" ]
        Just header ->
          concat
            [ fillToLength longestTime (printTime time)
            , " |  "
            , fillToLength longestHead header
            , contents ]
  in
    unlines . map printElement $ elements
