{-# OPTIONS_GHC -Wall #-}
module Todo.Read (readTask, readTasks) where

import Todo.Data
import Control.Applicative
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.Text
import Control.Monad (join)
import Data.List (intercalate)
import Data.Text (pack)
import Data.Time (parseTime)

readTasks :: String -> [Task]
readTasks = map readTask . lines

readTask :: String -> Task
readTask s = case parse parseTask "" (pack s) of
    Left e -> error (show e)
    Right t -> t

parseTask :: Parser Task
parseTask = do
        s <- stripSpace parseStatus
        d <- optional parseDate
        c <- stripSpace parseContent
        return $ Task s (join $ readDate <$> d) c

stripSpace :: Parser a -> Parser a
stripSpace = (many space *>)

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

readDate :: String -> Maybe Date
readDate = onDate parseTime
