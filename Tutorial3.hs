module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)

prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- ** Caesar Cipher Exercises

-- 1.
lookUp :: Char -> [(Char,Char)] -> Char
lookUp c list     | [ y | (x,y) <- list , x == c] == [] = c
                  | otherwise = [ y | (x,y) <- list , x == c] !! 0
                  

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec char [] = char
lookUpRec char ((a,b):tail)   | char == a = b
                              | otherwise = lookUpRec char tail


prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp char list = (lookUp char list) == (lookUpRec char list)

-- 2.
encipher :: Int -> Char -> Char
encipher int char = lookUpRec char (makeKey int)

-- 3.
normalise :: String -> String
normalise str = [ toUpper x | x <- str , isLetter x ]

normaliseRec :: String -> String
normaliseRec [] = []
normaliseRec (x:xs)     | isLetter x = toUpper x : normaliseRec xs
                        | otherwise = normaliseRec xs


prop_normalise :: String -> Bool
prop_normalise str = normalise str == normaliseRec str

-- 4.
enciphers :: Int -> String -> String
enciphers int str = [ encipher int x | x <- normalise str]

-- 5.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey list = [ (y,x) | (x,y) <- list] 

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((x,y):tail) = (y,x) : reverseKeyRec tail


prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey list = reverseKey list == reverseKeyRec list 

-- 6.
decipher :: Int -> Char -> Char
decipher int ' ' = ' '
decipher int char = encipher (26-int) char


decipher1 :: Int -> Char -> Char
decipher1 int char = lookUp char (reverseKey (makeKey int))





decipherStr :: Int -> String -> String
decipherStr int str = [decipher1 int x | x <- str , isLetter x , isUpper x]


-- ** Optional Material

-- 7.
candidates :: String -> [(Int, String)]
candidates str = [ (int,decipherStr int str) | int <- [1..26] , (isInfixOf "AND" (decipherStr int str)) || (isInfixOf "THE" (decipherStr int str))  ]


splitEachFive :: String -> [String]
splitEachFive xs | length xs > 5 = take 5 xs : splitEachFive (drop 5 xs)
                 | otherwise     = [ fillToFive xs ]

fillToFive :: String -> String
fillToFive xs = xs ++ replicate (5 - length xs) 'X'

-- An alternative solution demonstrating 'repeat'
fillToFive' :: String -> String
fillToFive' xs = take 5 (xs ++ repeat 'X')

-- The following example shows why 'transpose' is not
--  invertible in general. The transpose function
--  takes the 'columns' of a list of lists, and makes
--  them the 'rows' of a new list of lists. 
--
-- [[o n e],           [[o t t f f],       [[o n e e e],      [[o n e t w ]
--  [t w o],            [n w h o i],        [t w o r],       
--  [t h r e e],   -->  [e o r u v],   -->  [t h r e],  
--  [f o u r],          [e r e],            [f o u], 
--  [f i v e]   ]       [e],        ]       [f i v]     ]     

-- 8.
encrypt :: Int -> String -> String
encrypt int str = concat(transpose(splitEachFive(enciphers int str)))

-- 9.
decrypt :: Int -> String -> String
decrypt int str = decipherStr int (concat(transpose(deconcat str (number str))))


deconcat :: String -> Int -> [String]

deconcat str int  | length str > int = take int str : deconcat (drop int str) int 
                  | otherwise = [str]

number :: String -> Int
number str = (length str) `div` 5



--deconcatlistcomp :: String -> [String]
