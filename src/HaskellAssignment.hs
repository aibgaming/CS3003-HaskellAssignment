module HaskellAssignment where
import Data.List

------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"
findFirst :: Eq a => (a -> Bool) -> [a] -> Found
findFirst needle haystack = case (findIndex needle haystack) of
  
  Just index -> (Match index)
  
  Nothing -> (NoMatch)

------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome candidate = candidate == reverse (candidate)
