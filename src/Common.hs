module Common where

import qualified Debug.Trace (trace)

-- Taken from https://stackoverflow.com/a/29307068/2125072
count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

dbg :: Show a => String -> a -> a
dbg str a = Debug.Trace.trace (str ++ ": " ++ show a) a
