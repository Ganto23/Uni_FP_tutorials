-- Informatics 1 - Functional Programming 
-- Class Test 2023

module ClassExam where

import Data.Char
import Test.QuickCheck

-- Problem 1

-- a

f :: String -> Int
f str = sum [ ord char | char <- str , isAlpha char]

-- b

g :: String -> Int
g [] = 0
g (x:xs)    | isAlpha x = (ord x) + (g xs)
            | otherwise = g xs

-- c

h :: String -> Int
h =  foldr (+) 0 . map (ord) . filter (isAlpha)

-- d

prop_fgh :: String -> Bool
prop_fgh str = (f str == g str) && (g str == h str)

-- Problem 2

-- a

c :: String -> String -> Bool
c str1 str2 = function1 (zip str1 str2)

function1 :: [(Char,Char)] -> Bool
function1 list = and [ a == b | (a,b) <- list , isAlpha a , isAlpha b]


-- b

d :: String -> String -> Bool
d [] str2 = True
d str1 [] = True
d (x:xs) (y:ys) | isAlpha x && isAlpha y = (x == y) && d xs ys
                | otherwise = d xs ys

-- c

prop_cd :: String -> String -> Bool
prop_cd str1 str2 = c str1 str2 == d str1 str2 
