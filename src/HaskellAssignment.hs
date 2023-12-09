-- Assignment 4: Don't Haskell Me!
-- Name: Autri Ilesh Banerjee

module HaskellAssignment where
import Data.List


-- The Found data type is the return type for the findFirst function. 
-- It has two variants/constructors: Match and NoMatch. 
-- The Match constructor takes a parameter that indicates at which index the match was found.
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"


-- ==================================================================================
-- Function Name:                 findFirst
-- Parameters:                    1.  A function, needle, with one parameter of type a that returns 'True' 
--                                if its argument is the item to find and 'False' otherwise: '(a -> Bool)'
--                                2.  A list, haystack, of elements of type a
-- Return Value:                  A 'Match' (found data type) which contains the index of the first element
--                                (in left-to-right order) of haystack that causes needle to return 'True'; 
--                                'NoMatch' if no element of haystack causes needle to return 'True'
-- Description/Other:             findFirst returns NoMatch when haystack is an empty list.
-- ==================================================================================
findFirst :: Eq a => (a -> Bool) -> [a] -> Found
findFirst needle haystack = case (findIndex needle haystack) of
  -- if findIndex returns 'Just index' then findIndex should evaluate to 'Match index'
  Just index -> (Match index)
  -- if findIndex returns 'Nothing' then findIndex should evaluate to 'NoMatch'
  Nothing -> (NoMatch)

-- ==================================================================================
-- Function Name:                   palindrome
-- Parameters:                      A string, 'candidate'
-- Return Value:                    'True', if candidate is a palindrome; 'False', otherwise
-- Descriptor/Other:                The empty string is a palindrome. Strings will contain only lowercase letters.  
-- ==================================================================================

palindrome :: [Char] -> Bool
palindrome candidate = candidate == reverse (candidate) -- reverse the string candidate and check if its equal to the original candidate
