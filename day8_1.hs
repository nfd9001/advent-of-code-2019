import Data.List
import Data.List.Split -- package split
main = do
    f <- readFile "day8.txt"
    let layers = chunksOf 150 (head $ lines f)
    let s = head $ sortOn (length . filter (=='0')) layers 
    let o = length $ filter (=='1') s
    let t = length $ filter (=='2') s
    print $ o * t

    
