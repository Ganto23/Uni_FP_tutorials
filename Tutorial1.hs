module Tutorial1 where

import PicturesSVG -- needed for the optional chess part
import Test.QuickCheck


-- 2.
double :: Int -> Int
double x = x + x

square :: Int -> Int
square x = x * x

-- 3.
isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = a^2 + b^2 == c^2

-- 4.
leg1 :: Int -> Int -> Int
leg1 x y = x^2 - y^2

leg2 :: Int -> Int -> Int
leg2 x y = 2 * y * x

hyp :: Int -> Int -> Int
hyp x y = x^2 + y^2

-- 5.
prop_triple :: Int -> Int -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)

-- 5a  The function takes in two integer values and outputs a boolean. The integer values are used inside to create the pythagorean triple by using previous 
--      functions. Then it uses isTriple to check if its actually a pythagorean triple and output a boolean value. it should always be true.

-- 5b It should be true for any values entered. It works for the tested values.

-- 5c It passed the tests

-- 8.
pic1 :: Picture
pic1 = undefined

pic2 :: Picture
pic2 = undefined

-- ** Functions

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)

-- 9.
twoAbove :: Picture -> Picture
twoAbove x = undefined

fourPictures :: Picture -> Picture
fourPictures x = undefined

-- 10.
-- a)
emptyRow :: Picture
emptyRow = undefined

-- b)
otherEmptyRow :: Picture
otherEmptyRow = undefined

-- c)
middleBoard :: Picture
middleBoard = undefined

-- d)
whiteRow :: Picture
whiteRow = undefined

blackRow :: Picture
blackRow = undefined

-- e)
populatedBoard :: Picture
populatedBoard = undefined
