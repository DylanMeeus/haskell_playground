import System.IO
import Data.List
import Data.Char 


euler8 :: [String] -> Int
euler8 c = maximum [foldl1 (*) $ map digitToInt x | x <- parseData c]

parseData :: [String] -> [String]
parseData d = [take 13 x | x <- ps, length x >= 13]
       where ps = tails $ concat d 

main = do
     content <- readFile "problem8.txt"
     putStrLn $ show (euler8 $ lines content)