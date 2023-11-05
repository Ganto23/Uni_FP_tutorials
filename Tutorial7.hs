module Tutorial7 where

import LSystem
import Test.QuickCheck

pathExample = Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30

-- 1a. copy
copy :: Int -> Command -> Command
copy 0 command = Sit
copy int command = command :#: copy (int-1) (command)

-- 1b. polygon
polygon :: Distance -> Int -> Command
polygon dist int =  copy int (Go dist :#: Turn (360.0/fromIntegral int))

-- 2. snowflake
snowflake :: Int -> Command
snowflake x = copy 3 (f x :#: n :#: n)
    where
    f 0 = Go 10
    f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: p :#: f (x-1)
    n   = Turn 60
    p   = Turn (-60)

-- 3. sierpinski
sierpinski :: Int -> Command
sierpinski x =  f x
    where
    f 0 = GrabPen red :#: Go 10
    f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
    g 0 = GrabPen blue :#: Go 10
    g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f (x-1)
    n   = Turn 60
    p   = Turn (-60)
     
-- 4. hilbert
hilbert :: Int -> Command
hilbert =  undefined

-- 5. dragon
dragon :: Int -> Command
dragon =  undefined

-- ** Optional Material

-- 6a. split
split :: Command -> [Command]
split (Sit)     = []
split (x :#: y) =  split x ++ split y
split (x)       = [x]


-- 6b. join
join :: [Command] -> Command
join (x : xs) = x :#: join xs
join [] = Sit

-- 6c. equivalent
equivalent :: Command -> Command -> Bool
equivalent c1 c2 =  split c1 == split c2

-- 6d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c =  equivalent (join (split c)) (c)

prop_split :: Command -> Bool
prop_split commands = (not (Sit `elem` split commands)) && check1 (split commands)
    where
    check1 ((a :#: b) : ys) = False
    check1 (a : ys)         = check1 ys
    check1 []               = True


-- 7. optimise
optimise :: Command -> Command

optimise (x :#: (Go 0))                = optimise (x)
optimise ((Go 0) :#: y)                = optimise (y)

optimise (x :#: (Turn 0))              = optimise (x)
optimise ((Turn 0) :#: y)              = optimise (y)

optimise ((Go d) :#: (Go e) :#: y)     = optimise (Go (d+e) :#: y)
optimise (x :#: (Go d) :#: (Go e))     = optimise (x :#: Go (d+e))
optimise ((Go d) :#: (Go e))           = optimise (Go (d+e))

optimise ((Turn a) :#: (Turn b) :#: y) = optimise (Turn (a+b) :#: y)
optimise (x :#: (Turn a) :#: (Turn b)) = optimise (x :#: Turn (a+b))
optimise ((Turn a) :#: (Turn b))       = optimise (Turn (a+b))

optimise (x :#: y)                     = optimise (optimise x :#: optimise y)
optimise x                             = x


optimiser1 :: Command -> Command 
optimiser1 command =  optimise (joinBetter (splitBetter command))


splitBetter :: Command -> [Command]
splitBetter (Go 0)    = []
splitBetter (Turn 0)  = []
splitBetter (Sit)     = []
splitBetter (x :#: y) =  splitBetter x ++ splitBetter y
splitBetter (x)       = [x]

joinBetter :: [Command] -> Command
joinBetter (x : xs) = x :#: joinBetter xs
joinBetter [] = Go 0





