import Data.Char
import Data.List
import Data.Ord
import Data.Numbers.Primes
import Data.Number.CReal
import Numeric

-- I should split off the helpers to another file probably

isInt x = x == fromInteger (round x)


-- findDigitSum: Calculates the sum of the digits
findDigitSum :: Integral a => a -> a
findDigitSum a
	     | a < 10 = a
	     | otherwise = ((mod a 10) + findDigitSum (div a 10))
	   
-- change to ignore the last number for euler 80
findDigitSum' :: Integral a => a -> a
findDigitSum' a 
	      | a < 10 = 0
	      | otherwise = ((mod a 10) + findDigitSum' (div a 10))

-- EULER PROBLEMS
euler56 = maximum [findDigitSum x | x <- xs]
	  where xs = [a^b | a <- [1..100], b <- [1..100]]


-- euler80 : sum of digits of first 100 square 

euler80 = sum([findDigitSum (floor((sqrt x * 10^99) :: CReal)) | x <- xs])
	  where xs = [1..100] \\ [x^2 | x <- [1..10]]

-- euler62 cubic permutations
-- find permutations
-- check for each if it is a cube

--isCube x = (round (fromIntegral x ** (1/3))) ^3 == x 
--euler62 = [s | s <- xs, isCube s]
--where xs = [map (read :: String -> Int) (permutations k) | k <- [show x | x <- [41063625]]] --list of permutations



-- palindrome in base 2 and base10
euler36 = sum [fst m | m <- [(x,k) | x <- [1..1000000], let k = showIntAtBase 2 intToDigit x "", k == reverse k, show x == reverse (show x)]]



-- euler7; first one with primes

euler7 = [x | x <- [1..5], isPrime x]

-- generate fibonacci sequence
-- get first 9 and last 9 digits
-- check for pandigital ness

--isPandigital :: CReal -> Bool
isPandigital x = length (nub (intersect (show x) ['1'..'9'])) == 9

-- lastNine :: Int -> Int
lastNine x = mod x 1000000000

-- firstNine :: CReal -> CReal
firstNine x = read (take 9( show x)) :: CReal

euler104 = head ([ x | x <- xs, isPandigital (firstNine (fst x)), isPandigital (lastNine (fst x))])
	 where xs = zip fib [1..]
	 

fib = 0 : scanl (+) 1 fib


test = [ (x, elemIndex x xs) | x <- xs]
     where xs = take 10 fib


---------- PROBLEM 33, non trivial divisions with 2 digits for which the result is the same as canceling part of the number
-- 49 / 98 = 4/8

euler33 = [(fx, sx) | x <- xs, let t = (show (fst x) \\ show (snd x)) ++ (show (snd x) \\ show (fst x)), let r = read t :: Int, length t == 2, let fp = (div r 10), let lp = (mod r 10), lp /= 0, (fst x) / (snd x) == (fromIntegral fp / fromIntegral lp), let fx = floor (fst x), let sx = floor (snd x), mod fx 10 /= 0, mod sx 10 /= 0, fx > sx]
	where xs = [ (x,y) | x <- [10..99], y <- [10..99]]


-----------------
--- reversible numbers below 1 billion
--- 120 below 1000
--- reversible = number + reverse = number of only add digits

allDigitOdds :: Int -> Bool
allDigitOdds x
	     | x < 10 = odd x
	     | otherwise = odd (mod x 10) && allDigitOdds (div x 10) 

euler145 = length ([x | x <- xs])
	 where xs = [sum | x <- [1..floor(10**9)], let sum = (x + read (reverse (show x)) :: Int), odd sum, (mod x 10) /= 0, allDigitOdds sum] 

---------
--euler357
---------

--getDivisors :: Integral -> []
getDivisors a = [x | x <- [1..(div a 2)], (rem a x) == 0]

-- If the array is empty, let's just say it's true. 
allPrimes x [] = True 
allPrimes x (y:ys) = isPrime (y + div x y) && allPrimes x ys

euler357 = sum ([x | x <- xs, even x, allPrimes x (getDivisors x)])
	   where xs = map pred ps


euler357' = sum (1:[x | x <- xs, even x, allPrimes' x (getDivisors x)])
	  where xs = map pred ps
	    


ps = takeWhile (< 1000000) primes


allPrimes' x [] = True
allPrimes' x (y:ys) = elem (y + div x y) ps && allPrimes' x ys


------
--euler47: 4 consecutive numbers with 4 distinct prime factors
------

distinctPrimeLength :: Int -> Int
distinctPrimeLength x = length (nub (primeFactors x))

euler47 = head [x | x <- xs, elem (x+1) xs, elem (x+2) xs, elem (x+3) xs]	 
	  where xs = [x | x <- [1..floor(10**6)], distinctPrimeLength x == 4]


----
-- experiment to get the divisor count
-- euler 500
----
---
getDivisorCount x = foldl1 (*) [length y+1 | y <- (group (primeFactors x))] 
---
euler500 = mod (head [x | x <- xs, getDivisorCount x == floor(2**500500)]) 500500507
	 where xs = [500500507..]
---



totientPrimesLength :: Int -> Int
totientPrimesLength n = length [x | x <- [1..n-1], gcd x n ==1]

isAlwaysDecreasing :: String -> Bool
isAlwaysDecreasing (x:y:[]) = (digitToInt x) >= (digitToInt y)
isAlwaysDecreasing (x:y:xs) = (digitToInt x) >= (digitToInt y) && isAlwaysDecreasing (y:xs)


isAlwaysIncreasing :: String -> Bool
isAlwaysIncreasing (x:y:[]) = (digitToInt x) <= (digitToInt y)
isAlwaysIncreasing (x:y:xs) = (digitToInt x) <= (digitToInt y) && isAlwaysIncreasing (y:xs)

isBouncy :: Integer -> Bool
isBouncy x = not (isAlwaysIncreasing y) && not (isAlwaysDecreasing y)
	 where y = show x

bouncyIndex :: Integer -> Integer
bouncyIndex x = toInteger $ (length $ takeWhile (/= x) bouncyNumbers) + 1

-- keep taking the next bouncynumber, until length of bouncyNumbers == 50% bouncyNumber

euler112 = head [(x,z) | x <- bouncyNumbers, let y = bouncyIndex x, let z = (fromIntegral y / fromIntegral x) * 100, z >= 99]

-- x = 99, u = upper, l = lower, s = step




euler112' = [ (x,y,z) | x <- [1555000..1600000], let y = length $ takeWhile(<x) bouncyNumbers, let z = (fromIntegral y / fromIntegral x) * 100, z == 99]

bouncyNumbers = [x | x <- xs, isBouncy x]
	 where xs = [100..]


-- SPIRAL primes n == layer

rightTopPrimes n = take n [ (4 * (qn**2)) - 2*qn + 1 | qn <- [1..]]

leftBottomPrimes n = take n [ (4*(qn**2)) - 2*qn + 1 | qn <- [-1,-2..]]  

leftTopPrimes n = take n [ (x**2)+1 | x <- [2,4..]]
	      
rightBottomPrimes n = take n [ (x**2) | x <- [1,3..]]

getPrimesAtLayer :: Int -> [Int]
getPrimesAtLayer n = floor (last (rightTopPrimes n)) : floor (last (leftBottomPrimes n)) : floor (last (leftTopPrimes n)) : floor( last (rightBottomPrimes n)) : []

--primeRatio :: [a] -> Float
primeRatio x = (fromIntegral (length pxs) / fromIntegral (length x))*100
	   where pxs = [p | p <- x, isPrime p]
 

solveEuler58 x = if null solution
	       	    then solveEuler58 (x+5000)
		    else solution
	where solution = euler58 x	  	  

-- an array combined of the previous ones
euler58 l = head [e | e <- scanl1 (++) [getPrimesAtLayer x | x <- [1..l]], primeRatio e < 10]


testing a = if b > 20
	    then b
	    else testing (a+1)
	where b = a + 10


---
-- permutations that are double-pandigital and divisble by 11

euler491 = length [x | x <- xs]
	 where xs = permutations "40561817703823564929" 


-- BELOW is the MAIN function.
main = print euler491