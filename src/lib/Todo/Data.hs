{-# OPTIONS_GHC -Wall #-}
module Todo.Data ( Task(..), TodoStatus(..)
                 , Priority, Date, Content
                 , priority, completeDate
                 , onDate, dateFormat
                 ) where

import Data.Time (UTCTime)
import System.Locale (TimeLocale, defaultTimeLocale)

type Priority = Char
type Date = UTCTime
type Content = String

data TodoStatus = Todo (Maybe Priority)
                | Done (Maybe Date)
                deriving Show

data Task = Task
          { status :: TodoStatus
          , maybeDate :: Maybe Date
          , content :: String
          } deriving Show

priority :: Task -> Maybe Priority
priority t = case status t of
    Todo (Just p) -> Just p
    _ -> Nothing

completeDate :: Task -> Maybe Date
completeDate t = let st = status t in case st of
    Done d -> d
    _ -> Nothing

onDate :: (TimeLocale -> String -> a -> b) -> a -> b
onDate f = f defaultTimeLocale dateFormat

dateFormat :: String
dateFormat = "%Y-%m-%d"
