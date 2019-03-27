module Main where

import           Data.List.Split    (splitOn)
import           Data.Maybe         (mapMaybe)
import           System.Environment (getArgs)
import           Text.Read          (readMaybe)

data Candidate =
  Candidate { constituency :: String
            , party        :: String
            , sex          :: String
            , averageAge   :: Double }
               deriving Show

parser :: String -> Maybe Candidate
parser line = case splitOn ',' line of
                   [c,p,s,a] -> Just (Candidate (read c) (read p) (read s) (read a))
                   _ -> Nothing

fullParser :: String -> [Candidate]
fullParser = mapMaybe parser . tail . lines

handler :: FilePath -> IO ()
handler path = do
  f <- readFile path
  mapM_ print (fullParser f)

main :: IO ()
main = do
  args <- getArgs
  case args of
       [file] -> handler file
       _      -> putStrLn "Call with FILE"
