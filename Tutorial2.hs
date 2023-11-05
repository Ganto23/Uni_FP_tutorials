module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck


-- 1. inRange

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [ x | x <- xs , x >= lo , x <= hi]


-- 2. multDigits

multDigits :: String -> Int
multDigits str = product[ fromEnum x - 48 | x <- str , isDigit x]

countDigits :: String -> Int
countDigits str = length[ x | x <- str , isDigit x]

prop_multDigits :: String -> Bool
prop_multDigits str = multDigits str <= 9^countDigits str


-- 3. capitalise and title

capitalise :: String -> String
capitalise word = toUpper(head word) : [ toLower c | c <- tail word, isAlpha c ]
-- toUpper head x : map toLower tail x

lowercase :: String -> String
lowercase word = [toLower c | c <- word]

title :: [String] -> [String]
title words = capitalise(head words) : [if length c>3 then capitalise c else lowercase c | c <- tail words ]

-- 4. score and totalScore

score :: Char -> Int
score x
    | x `elem` ['A','E','I','O','U'] = 3
    | x `elem` ['a','B','C','D','e','F','G','H','i','J','K','L','M','N','o','P','Q','R','S','T','u','V','W','X','Y','Z'] = 2
    | x `elem` ['b','c','d','f','g','h','j','k','l','m','n','p','q','r','s','t','v','w','x','y','z'] = 1
    | otherwise = 0

totalScore :: String -> Int
totalScore xs = product[ score i | i <- xs, isAlpha i]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs >= 1


-- ** Optional Material

-- 5. crosswordFind

crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [ x | x <- words, length x == len && letter == x!!pos]


-- 6. search

search :: String -> Char -> [Int]
search str goal = [ i | (x,i) <- zip str [0..] , x == goal]

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = undefined

