{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Todo where

import Control.Applicative
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.Text
import Control.Monad (join)
import Data.List (intercalate)
import Data.Text (pack)
import Data.Time (UTCTime, parseTime, formatTime)
import System.Locale (TimeLocale, defaultTimeLocale)

type Priority = Char
type Date = UTCTime
type Content = String
type Project = String
type Context = String
type TaskStore = [Task]

data TodoStatus = Todo (Maybe Priority)
                | Done (Maybe Date)
                deriving Show

data Task = Task
          { status :: TodoStatus
          , maybeDate :: Maybe Date
          , content :: String
          } deriving Show

showStatus :: Task -> String
showStatus t = case status t of
    Todo (Just p) -> '(' : p : ")"
    Done x -> "x " ++ case x of
        Just d -> showDate d
        _ -> ""
    _ -> ""

showTask :: Task -> String
showTask t = merge [st, date, text]
  where
    merge = unwords . skipEmpty
    skipEmpty = filter (not . null)
    st = showStatus t
    date = case maybeDate t of
        Just d -> showDate d
        _ -> ""
    text = content t

priority :: Task -> Maybe Priority
priority t = case status t of
    Todo (Just p) -> Just p
    _ -> Nothing

complete :: Task -> Bool
complete t = case status t of
    Done _ -> True
    _ -> False

completeDate :: Task -> Maybe Date
completeDate t = let st = status t in case st of
    Done d -> d
    _ -> Nothing

emptyTask :: Task
emptyTask = Task (Todo Nothing) Nothing ""

readTask :: String -> Task
readTask s = case parse parseTask "" (pack s) of
    Left e -> error (show e)
    Right t -> t

stripSpace :: Parser a -> Parser a
stripSpace = (many space *>)

parseTask :: Parser Task
parseTask = do
        s <- stripSpace parseStatus
        d <- optional parseDate
        c <- stripSpace parseContent
        return $ Task s (join $ readDate <$> d) c

parseStatus :: Parser TodoStatus
parseStatus = (Done <$> parseComplete) <|> (Todo <$> optional parsePriority)

parsePriority :: Parser Priority
parsePriority = try $ char '(' *> upper <* char ')' <* some space

parseComplete :: Parser (Maybe Date)
parseComplete = try $ do
        _ <- char 'x'
        _ <- char ' '
        d <- optional parseDate
        return (join $ readDate <$> d)

parseDate :: Parser String
parseDate = try date
  where
    date = do
        _ <- many space
        year <- count 4 digit
        _ <- char '-'
        month <- count 2 digit
        _ <- char '-'
        day <- count 2 digit
        _ <- char ' '
        return (intercalate "-" [year, month, day])

parseContent :: Parser Content
parseContent = many anyChar

dateFormat :: String
dateFormat = "%Y-%m-%d"

onDate :: (TimeLocale -> String -> a -> b) -> a -> b
onDate f = f defaultTimeLocale dateFormat

showDate :: Date -> String
showDate = onDate formatTime

readDate :: String -> Maybe Date
readDate = onDate parseTime
