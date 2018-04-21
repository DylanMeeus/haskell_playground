import System.IO
import System.Environment
import Data.List
import Data.Char

-- parse in 2 steps (first scan for variables, then process text)


data Variable = Variable {
    identifier :: String,
    value :: String
} deriving (Show)

type Variables = [Variable]
type Codelines = [String]
type Codeline = String
type Output = String


-- region <remove spaces>
strip_trailing_leading_space :: String -> String
strip_trailing_leading_space input = strip_trailing_space  $ strip_leading_space input

strip_trailing_space :: String -> String
strip_trailing_space input = reverse $ dropWhile(\c -> isSpace c) $ reverse input

strip_leading_space :: String -> String
strip_leading_space input = dropWhile(\c -> isSpace c) input   
-- endregion

-- starts_with_ignore_space "    @" '@' -> True
starts_with_ignore_space :: String -> Char -> Bool
starts_with_ignore_space [] _ = False
starts_with_ignore_space inp char = (length first_without_space > 0) && head first_without_space == char
    where first_without_space = strip_leading_space inp

-- remove lines starting with @
remove_comments :: Codelines -> Codelines
remove_comments codeline = filter(\x -> not (starts_with_ignore_space x '@')) codeline 

-- remove empty lines from the code
remove_empty_lines :: Codelines -> Codelines
remove_empty_lines codelines = filter(\x -> x /= []) codelines


-- split on '=' and remove the ' ' chars around the variable
create_variable :: Codeline -> Variable
create_variable codeline = Variable identifier value
    where 
        identifier = takeWhile(\x -> x /= ' ') $ strip_leading_space codeline
        value = takeWhile(\x -> x /= '\'') $ tail $  dropWhile(\x -> x /= '\'') codeline 


extract_text :: String -> Int -> Int -> String
extract_text input start stop = drop (start + 1) . take stop $ input

find_pipes :: String -> [Int]
find_pipes input = map(\t -> snd t) $ filter(\t -> fst t == '|') char_index 
    where char_index = zip input [0..length input]


textloc_scan_pipes :: String-> [Int] -> [Output]
textloc_scan_pipes _ [] = []
textloc_scan_pipes codeline (x:y:xs) = [extract_text codeline x y] 
                                    ++ textloc_scan_pipes codeline xs

textloc_scan :: Codelines -> [Output]
textloc_scan codelines = textloc_scan_pipes joined_text pipes  
    where pipes = find_pipes $ unlines codelines 
          joined_text = unlines codelines

-- scan for variable
variable_scan :: Codelines -> Variables 
variable_scan [] = []
variable_scan codelines = map(\x -> create_variable x) loc_variables 
    where loc_variables = filter(\x -> starts_with_ignore_space x '_') codelines


-- remove lines we don't need
sanitize :: Codelines-> Codelines
sanitize codelines = init $ tail -- remove the first and last line (rule enclosures)  
                     $ remove_comments 
                     $ remove_empty_lines x
        where x = map(\y -> strip_trailing_leading_space y) codelines

parse :: String -> String
parse input = intercalate " " $ loc 
    where loc = textloc_scan $ sanitize $ lines input


main = do
    content <- readFile "rule.dcg"
    putStrLn $ parse content 
