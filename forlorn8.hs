import Data.Char
import Data.List

e'' :: Int -> [Char] -> [[Char]]
e'' n [] = []
e'' n str
    | (length str) < 4 = []
    | otherwise = (take n str) : e'' n (drop n str)

e' :: Int -> [Int] -> [Char] -> [[[Char]]]
e' _ [] _ = []
e' n (x:xs) str = (e'' n (drop x str)) : (e' n xs str)

e :: Int -> [Char] -> [[Char]]
e n str = filter (\x -> not $ '0' `elem` x) (concat (e' n [0..(n-1)] str))

s :: [[Char]] -> [([Int], Int)]
s [] = []
s (x:xs) = (y, product y) : s xs
    where y = map digitToInt x

m :: [([Int], Int)] -> ([Int], Int)
m ls= maximumBy (\(_,x) (_,y)-> compare x y) ls


solution :: [Char] -> ([Int], Int)
solution i = m . s . e 13 $ i

main = do
    content <- readFile "problem8.txt"
    let input = lines content
    putStrLn $ show (solution (concat input))