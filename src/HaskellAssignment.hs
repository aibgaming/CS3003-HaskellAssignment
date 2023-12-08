

-- Assignment 4: Don't Haskell Me!

-- Name: Anay Abhijit Joshi
-- M-ID: M14391356
-- Date: 12/8/2023 (Friday)

-- This file has the required implementations of the functions specified below:
-- (findFirst and palindrome).
-------------------------------------------------------------------------------------------

module HaskellAssignment where
-- Importing the necessary module(s) :- (Data.list) for findIndex function
import Data.List

------------------------------------------------
-- NOTE: These comments are taken from the assignment's module given by Professor Will Hawkins

-- Name:                findFirst
-- Parameters:          1.  A function, needle, with one parameter
--                      of type a that returns 'True' if its argument
--                      is the item to find and 'False' otherwise: '(a -> Bool)'
--                      2.  A list, haystack, of elements of type a
-- Return Value:        A 'Match' which contains the index of the first element
--                      (in left-to-right order) of haystack that causes needle 
--                      to return 'True'; 'NoMatch' if no element of haystack causes 
--                      needle to return 'True'
-- Descriptor/Other:    findFirst returns NoMatch when haystack is an empty list.
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"
findFirst :: Eq a => (a -> Bool) -> [a] -> Found

-- Here, "findIndex" will return the corresponding index (referred to haskell.org and stackoverflow.com for syntax)
-- Using the case expression for pattern matching
findFirst needle haystack = case (findIndex needle haystack) of
  -- if findIndex returns 'Just index' then 'Match index'
  Just index -> (Match index)
  -- if findIndex returns 'Nothing' then 'NoMatch'
  Nothing -> (NoMatch)

maybe (NoMatch) (Match index) (findIndex needle haystack of)


------------------------------------------------
-- NOTE: These comments are taken from the assignment's module given by Professor Will Hawkins

-- Name:                  palindrome
-- Parameters:            A string, 'candidate'
-- Return Value:          'True', if candidate is a palindrome; 'False', otherwise
-- Descriptor/Other:      The empty string is a palindrome. Strings will contain only lowercase letters.  
------------------------------------------------
palindrome :: [Char] -> Bool
-- The reverse function creates a new string from the original one with items in the reverse order (referred to haskell.org for syntax)
-- This function should be very similar to the module's given code ("equals1 x = x == 1"), because it would just check if the string 'candidate'
-- is equal to its reverse (i.e., palindrome) 
palindrome candidate = candidate == reverse (candidate)

