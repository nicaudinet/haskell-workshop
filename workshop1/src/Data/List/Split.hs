module Data.List.Split where

import           Data.List          (unfoldr)

splitOn :: Char -> String -> [String]
splitOn predicate = unfoldr (go predicate)
  where
    go :: Char -> String -> Maybe (String, String)
    go _step "" = Nothing
    go step (h: acc) | h == step = go step acc
    go step acc = let (x,rest) = break (== step) acc
                       in Just (x, rest)
