module Data.List.Split where

import           Data.List          (unfoldr)

splitOn :: Char -> String -> [String]
splitOn predicate = unfoldr (go predicate)
  where
    go :: Char -> String -> Maybe (String, String)
    go _predicate "" = Nothing
    go predicate (h: acc) | h == predicate = go predicate acc
    go predicate acc = let (x,rest) = break (== predicate) acc
                       in Just (x, rest)
