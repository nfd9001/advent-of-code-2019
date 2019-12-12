import Data.List
import Data.List.Split -- package split

p '0' _ = '0'
p '1' _ = '1'
p '2' x = x

r '0' = '■'
r '1' = '□'
r _ = ' '

pp top bot = (uncurry p) <$> zip top bot

main = do
    f <- readFile "day8.txt"
    let layers = chunksOf 150 (head $ lines f)
    let l = foldl1' pp layers
    let v = r <$> l
    let ls = chunksOf 25 v
    let p = putStrLn <$> ls
    sequence p
    
