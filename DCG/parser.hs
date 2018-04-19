import System.IO
import System.Environment
import Data.List
import Data.Char

-- parse in 2 steps (first scan for variables, then process text)


data Variable = Variable {
    identifier :: String,
    value :: String
}

type Variables = [Variable]
type Codelines = [String]


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


-- remove lines we don't need
sanitize :: Codelines-> Codelines
sanitize codelines = init $ tail -- remove the first and last line (rule enclosures)  
                     $ remove_comments 
                     $ remove_empty_lines x
        where x = map(\y -> strip_trailing_leading_space y) codelines

parse :: String -> String
parse input = intercalate " " loc 
    where loc = sanitize $ lines input


main = do
    content <- readFile "rule.dcg"
    putStrLn $ parse content 
