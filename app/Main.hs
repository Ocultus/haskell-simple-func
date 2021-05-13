module Main where

import Lib

--1
sign :: Integer -> Integer
sign x = 
    if x > 0 then 1
    else if x == 0 then 0
    else -1

--2
lenVec :: (Double, Double, Double) -> (Double, Double, Double) -> Double
lenVec (x1, y1, z1) (x2, y2, z2) = 
    sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) + (z2 - z1) * (z2 -z1))

--3
flip1 :: (a -> b -> c) -> b -> a -> c
flip1 f x y = f y x

--4
findNode :: Integer -> Integer -> Integer
findNode x y = if x == 0 then y
               else if y == 0 
               then x
               else if x > y 
               then findNode (x `mod` y) y
               else findNode x (y `mod` x) 

--5
isPrime :: Integer -> Bool
isPrime x | x <= 1      =   False
          | otherwise   =   isPrime' x 2
    where isPrime' x i  =   if x `mod` i == 0 
                            then False
                            else if i * i > x 
                            then True
                            else isPrime' x (i+1)

--6
calcE :: Double -> Double -> Double
calcE x eps = helper x eps x 0 1 2
    where helper x eps xn tempEps value i = if (eps > tempEps) && (value /= 1)
                                            then value
                                            else helper x eps (xn*x/i) (newValue - value) newValue (i+1)
            where newValue = value + xn 


--7
(|-|) x y = abs x - abs y

--8
--logBase 4 $ min 20 $ 9 + 7


--9
doubleFact :: Integer -> Integer
doubleFact n = helper 1 n
    where helper acc n  | n > 1 = helper (acc * n) (n -2)
                        | otherwise = acc  

--10
seqA :: Integer -> Integer
seqA n = helper 3 2 1 3 n
    where helper ak2 ak1 ak i n = if n == 0 
                                    then ak
                                    else if n == 1
                                    then ak1
                                    else if n == 2
                                    then ak2
                                    else if i == n
                                    then ak2 + ak1 - 2*ak
                                    else helper (ak2 + ak1 - 2*ak) ak2 ak1 (i+1) n 



--11,12
fibonacci :: Integer -> Integer
fibonacci n = if n > 1 
              then helperPlus 2 n 1 0
              else helperMin (-1) n 0 1 
    where helperMin i n fibn1 fibn2 =   if n == 0
                                        then 0
                                        else if n == 1 
                                        then 1
                                        else if i == n
                                        then fibn2 - fibn1
                                        else helperMin (i-1) n (fibn2 - fibn1) fibn1
          helperPlus i n fibn1 fibn2 = if n == 0
                                        then 0
                                        else if n == 1 
                                        then 1
                                        else if i == n
                                        then fibn1 + fibn2
                                        else helperPlus (i+1) n (fibn1 + fibn2) fibn1


--13
sumAndCount :: Integer -> (Integer, Integer)
sumAndCount x = helper x 0 0 
    where helper x sum count =  if x == 0 
                                then (sum, count)
                                else helper (x `quot` 10) (sum + (x `mod` 10)) (count+1)



--14
integration :: (Double -> Double) -> Double -> Double -> Double
integration func a b = h/2 * (func a + func b + 2 * helper func a 1 h 1000 0)
    where h = (b-a)/1000
          helper func b i h n value =   if i == n
                                        then value
                                        else helper func b (i+1) h 1000 (value + func (a + i * h))


main = putStrLn(show(integration sin pi 0))