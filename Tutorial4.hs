module Tutorial4 where

import Data.Char
import Data.List
import Test.QuickCheck
import Data.Ratio


-- 1. doubles
-- a.
doublesComp :: [Int] -> [Int]
doublesComp xs = [ 2 * x | x <- xs] 

-- b.
doublesRec :: [Int] -> [Int]
doublesRec [] = []
doublesRec (x:xs) =  2 * x : doublesRec xs

-- c.
doublesHO :: [Int] -> [Int]
doublesHO =  map (* 2)

-- d.
prop_doubles :: [Int] -> Bool
prop_doubles xs =  (doublesComp xs == doublesRec xs) && (doublesRec xs == doublesHO xs)

-- 2. aboves
-- a.
abovesComp :: Int -> [Int] -> [Int]
abovesComp x ys =  [ y | y <- ys , y > x]

-- b.
abovesRec :: Int -> [Int] -> [Int]
abovesRec x [] = []
abovesRec int (x:xs)    | x > int = x : abovesRec int xs
                        | otherwise = abovesRec int xs

-- c.
abovesHO :: Int -> [Int] -> [Int]
abovesHO int=  filter (> int)

-- d.
prop_aboves :: Int -> [Int] -> Bool
prop_aboves x xs =  (abovesComp x xs == abovesRec x xs) && (abovesRec x xs == abovesHO x xs)

-- 3. parity
-- a.
xor :: Bool -> Bool -> Bool
xor bool1 bool2 = bool1 /= bool2

-- b.
parityRec :: [Bool] -> Bool
parityRec [] = True
parityRec (x:xs) = x `xor` (parityRec xs)

-- c.
parityHO :: [Bool] -> Bool
parityHO =  foldr (xor) True

-- d.
prop_parity :: [Bool] -> Bool
prop_parity listOfBool =  (parityRec listOfBool) == (parityHO listOfBool)

-- 4. allcaps
-- a.
allcapsComp :: String -> Bool
allcapsComp str =  and[ isUpper s | s <- str , isAlpha s]

-- b.
allcapsRec :: String -> Bool
allcapsRec [] =  True
allcapsRec (x:xs)   | isAlpha x = (isUpper x) && allcapsRec xs
                    | otherwise = allcapsRec xs

-- c.
allcapsHO :: String -> Bool
allcapsHO =  foldr (&&) True . map (isUpper) . filter (isAlpha)

-- d.
prop_allcaps :: String -> Bool
prop_allcaps str =  (allcapsComp str == allcapsRec str) && (allcapsRec str == allcapsHO str)


-- ** Optional material
-- Matrix manipulation

type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform xs = all (== head(xs)) xs 

-- b.
valid :: Matrix -> Bool
valid [] = False
valid [[]] = False
valid matrix = uniform(map (length) matrix)


-- 6.
width :: Matrix -> Int
width m = length (head m)

height :: Matrix -> Int
height m = length m

plusRow :: [Rational] -> [Rational] -> [Rational]
plusRow m n = zipWith (+) m n

recursiveThing :: Matrix -> Matrix -> Matrix
recursiveThing list [] = [[]]
recursiveThing [] list = [[]]
recursiveThing (x:xs) (y:ys) = (plusRow x y) : recursiveThing xs ys

plusM :: Matrix -> Matrix -> Matrix
plusM ms ns | (width ms) /= (width ns) = [[]]
            | (height ms) /= (height ns) = [[]]
            | otherwise = take (length(recursiveThing ms ns)-1) (recursiveThing ms ns)



-- 7.

timesM :: Matrix -> Matrix -> Matrix
timesM xs ys = [ dotProduct (xs !! a) ys | a <- [0..length xs - 1] , validMult xs ys ]


dotProduct :: [Rational] -> Matrix -> [Rational]
dotProduct xs yss = [ sum (dot xs (makeDots yss a)) | a <- [0..length (head yss) - 1]]



dot :: [Rational] -> [Rational] -> [Rational]
dot [] ys = []
dot xs [] = []
dot (x:xs) (y:ys) = x * y : dot xs ys

makeDots :: Matrix -> Int -> [Rational]
makeDots matrix column = [ (matrix !! y) !! column | y <- [0..length matrix-1]]

validMult :: Matrix -> Matrix -> Bool
validMult x [[]] = False
validMult [[]] y = False
validMult x y   | not (valid y) = False
                | not (valid x) = False
                | length (head x) /= length y = False
                | otherwise = True





-- ** Challenge

-- 8.
-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]     
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined
        
determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined        
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse1 :: Rational -> Property
prop_inverse1 a = undefined

prop_inverse2 :: Rational -> Rational -> Rational 
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)
        
prop_inverse3 :: Triple Rational -> 
                 Triple Rational -> 
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined
