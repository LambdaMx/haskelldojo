module StringCalculator ( 
 toInt, add, add2, add3, add4
) where

import Data.String.Utils
import Data.Maybe
import Text.Regex

-- Problem 1: Be able to add integers given in a string, separated by commas.
-- Just the following cases:
-- "a"     -> n
-- "a,b"   -> n+m
-- "a,b,c" -> a+b+c 

toInt :: [Char] -> Int
toInt xs
  | isJust (matchRegex intRegex xs) = (read xs) :: Int
  | otherwise = error $ xs ++ " is not an integer"
  where intRegex = mkRegex "^[-]?[0-9]+$"

add :: [Char] -> Int
add xs = foldl1 (+) $ map toInt $ split "," xs

-- Problem 2: Allow an undefined number of integers.
-- Examples: 
-- "1,2,3,4,5"       -> 15
-- "10,20,30,40,50"  -> 150
-- "3,3,3,3,3,3,3,3" -> 24

add2 = add -- We are happy! :D

-- Problem 3: Allow an optional parametrizable 1 character delimiter.
-- When given a delimiter, it should precede the string of integers with
-- the following format:
--       "//[delimiter]\n[string_of_integers_with_delimiter]"
-- where [delimiter] can be any character.
-- when given no delimiter, "," is assumed.

add3 :: [Char] -> Int
add3 ('/':'/':delim:'\n':xs) = mkAdd3 [delim] xs
add3                      xs = mkAdd3 "," xs

mkAdd3 delimiter = foldl1 (+) . map toInt . (split delimiter)

-- Problem 4: It should reject negative numbers.
-- When given one negative number within the list of integers. It should throw an error.
-- When given >1 negative numbers, it should return the list of all given negative numbers.

add4 :: [Char] -> Either Int [Int]
add4 ('/':'/':delim:'\n':xs) = addIntsFromStr [delim] xs
add4                      xs = addIntsFromStr "," xs

addIntsFromStr :: [Char] -> [Char] -> Either Int [Int]
addIntsFromStr delimiter xs
  | length listOfNegatives == 0  = Left $ foldl1 (+) $ listOfInts xs
  | length listOfNegatives == 1  = error $ "error: " ++ show (head listOfNegatives)
  | otherwise                    = Right listOfNegatives 
  where listOfInts xy   = map toInt . (split delimiter) $ xy
        listOfNegatives = filter (<0) $ listOfInts xs



