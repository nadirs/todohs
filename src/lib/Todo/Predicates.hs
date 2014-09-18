{-# OPTIONS_GHC -Wall #-}
module Todo.Predicates ( newerThan
                       , isComplete
                       ) where

import Todo.Data

newerThan :: Task -> Date -> Bool
newerThan t d = maybe False (d >) taskDate
  where
    taskDate = maybeDate t

isComplete :: Task -> Bool
isComplete t = case status t of
    Done _ -> True
    _ -> False
