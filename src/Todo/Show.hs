{-# OPTIONS_GHC -Wall #-}
module Todo.Show (showTask, showStatus, showDate) where

import Todo.Data
import Data.Time (formatTime)

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

showDate :: Date -> String
showDate = onDate formatTime
