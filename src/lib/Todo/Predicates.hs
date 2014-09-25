{-# OPTIONS_GHC -Wall #-}
module Todo.Predicates ( minDate
                       , maxDate
                       , minPriority
                       , maxPriority
                       , isComplete
                       ) where

import Todo.Data

minDate :: Date -> Task -> Bool
minDate d = maybe False (d >=) . maybeDate

maxDate :: Date -> Task -> Bool
maxDate d = maybe True (d >=) . maybeDate

minPriority :: Priority -> Task -> Bool
minPriority p = maybe False (>= p) . priority

maxPriority :: Priority -> Task -> Bool
maxPriority p = maybe True (<= p) . priority

isComplete :: Task -> Bool
isComplete t = case status t of
    Done _ -> True
    _ -> False
