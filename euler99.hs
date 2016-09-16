import Data.Ord
import Data.List
import System.IO
import System.Environment
import System.FilePath
import Text.Regex

-- Largest exponential 
-- This solution is in a separate file because it is the first one dealing with I/O
-- And I want to make this as clearly visible as possible.
-- as I have not done this before.

-- First we read in the file
-- Then we transform the file into a bunch of equations
-- find max (and note the line number)


parseLine :: String -> Float
parseLine x = read b * (log $ read a)
	  where split = splitRegex (mkRegex "[,]") x
	  	b = split!!1
	  	a = split!!0


euler99 :: [String] -> (Float,Integer)
euler99 i = maximumBy (comparing fst) $ zip [parseLine x | x <- i] [1..]

main = do
     content <- readFile "p099_base_exp.txt"
     let input = lines content
     putStrLn $ show (euler99 input)