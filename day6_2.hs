import Data.Map.Strict hiding (null, (\\))
import Data.List
import Data.Maybe
import Data.Tree

search val (Node label forest) =
    if val == label
    then [label]
    else fromMaybe [] $ do 
        path <- find (not . null) ((search val) <$> forest)
        return $ label:path

main = do
    f <- readFile "day6.txt"
    let l = lines f
    pairs <- return $ do
        l' <- l
        w <- return $ words $ (\x -> if x == ')' then ' ' else x) <$> l'
        return (w!!0,[w!!1])
    let map = fromListWith (++) pairs
    let tree = unfoldTree (\x -> (x, findWithDefault [] x map)) "COM"
    let y = search "YOU" tree 
    let s = search "SAN" tree
    print $ length ((y\\s) ++ (s\\y)) - 2
