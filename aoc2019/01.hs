import Data.List
import System.IO
import Control.Monad


main = do
    contents <- readFile  "01.txt"
    print . calc $ lines contents
    print . calc2 $ lines contents

calModuleWeight :: String -> Int
calModuleWeight x =
    weight `div` 3 - 2
    where weight = read x

calcRemainingWeight x
    | remaining < 0 = 0
    | otherwise = remaining + (calcRemainingWeight remaining)
    where remaining = x `div` 3 - 2

calc :: [String] -> Int
calc x = foldl' (+) 0 y
    where y = map calModuleWeight x
calc2 :: [String] -> Int
calc2 x = foldl' (+) 0 y
    where y = map calcRemainingWeight $ map read x
