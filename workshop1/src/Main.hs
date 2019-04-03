{-# LANGUAGE DeriveGeneric #-}
module Main where

import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as B
import           Data.List.Split      (splitOn)
import           Data.Maybe           (mapMaybe)
import           GHC.Generics         (Generic)
import           System.Environment   (getArgs)
import           Text.Read            (readMaybe)

-- | The domain model for the workshop
data Candidate =
  Candidate { constituency :: String
            , party        :: String
            , sex          :: String
            , averageAge   :: Double }
               deriving (Show, Generic)

instance JSON.ToJSON Candidate

-- | Parse a line of csv into candidate
parser :: String -> Maybe Candidate
parser line = case splitOn ',' line of
                   [c,p,s,a] -> fillCandidate (Candidate (stripQ c) (stripQ p) (stripQ s)) (readMaybe a)
                   _ -> Nothing

-- | Strip (first) quotation marks from the string
stripQ :: String -> String
stripQ = lstrip . rstrip
  where
    lstrip ('"':xs) = xs
    lstrip xs       = xs
    rstrip          = reverse . lstrip . reverse

-- | Complete a candidate if age is found
fillCandidate :: (Double -> Candidate) -> Maybe Double -> Maybe Candidate
fillCandidate f (Just double) = Just (f double)
fillCandidate _ _             = Nothing

-- | Parse a block of text into a list of candidates
fullParser :: String -> [Candidate]
fullParser = mapMaybe parser . tail . lines

-- | Read a file and print the json
handler :: FilePath -> IO ()
handler path = do
  contents <- readFile path
  B.putStr $ JSON.encode (fullParser contents)

main :: IO ()
main = do
  args <- getArgs
  case args of
       [file] -> handler file
       _      -> putStrLn "Call with FILE"
