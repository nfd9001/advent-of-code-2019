import Data.List
-- left as it was in my dir, since pt 1 should be obvious given this
input = show <$> [146810..612563] 

getpairs (x:y:xs) = (x,y) : getpairs (y:xs)
getpairs _ = []

check i = let
        pairs = getpairs i
    in
        if (any (==2) (length <$> group i)) && (all (uncurry (<=)) pairs) 
        then 1
        else 0

main = putStrLn $ show $ sum $ check <$> input 
