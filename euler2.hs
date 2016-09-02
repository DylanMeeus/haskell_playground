import Data.Char
import Data.List
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