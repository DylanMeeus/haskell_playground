-- imports
import Data.List as L;

-- end imports

euler1 = sum [x | x <- [3..999], mod x 3 == 0 || mod x 5 == 0];






euler29 = length (L.nub [fst x ^ snd x | x <- [(a,b) | a <- [2..100], b <- [2..100]]]);