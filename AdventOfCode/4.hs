import System.IO
import System.Environment
import Data.List
-- Get the valid passphrases (no repeated words)


file1 = "passphrase.txt"
solve :: [String] -> Int
solve input = length $ filter(\x -> length (words x) == length(nub (words(x)))) input

main :: IO()
main = do
       content <- readFile file1
       let fileLines = lines content
       putStrLn (show $ solve fileLines)
      
