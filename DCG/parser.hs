import System.IO
import System.Environment
import Data.List

-- parse in 2 steps (first scan for variables, then process text)


data Variable = Variable {
    identifier :: String,
    value :: String
}

type Variables = [Variable]
type Codeline = [String]


-- starts_with_ignore_space "    @" '@' -> True
starts_with_ignore_space :: String -> Char -> Bool
starts_with_ignore_space [] _ = False
starts_with_ignore_space inp char = (length first_without_space > 0) && head first_without_space == char
    where first_without_space = filter (\x -> x /= ' ') inp

-- remove lines starting with @
remove_comments :: Codeline -> Codeline
remove_comments codeline = filter(\x -> not (starts_with_ignore_space x '@')) codeline 

-- remove empty lines from the code
remove_empty_lines :: Codeline -> Codeline
remove_empty_lines codelines = filter(\x -> x /= []) codelines


-- remove lines we don't need
sanitize :: Codeline-> Codeline
sanitize codelines = init $ tail -- remove the first and last line (rule enclosures)  
                     $remove_comments 
                     $ remove_empty_lines codelines 

parse :: String -> String
parse input = intercalate " " loc 
    where loc = sanitize $ lines input


main = do
    content <- readFile "rule.dcg"
    putStrLn $ parse content 
