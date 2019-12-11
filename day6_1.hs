import Data.Map.Strict
import Data.Tree
main = do
    f <- readFile "day6.txt"
    let l = lines f
    pairs <- return $ do
        l' <- l
        w <- return $ words $ (\x -> if x == ')' then ' ' else x) <$> l'
        return (w!!0,[w!!1])
    let map = fromListWith (++) pairs
    let tree = unfoldTree (\x -> (x, findWithDefault [] x map)) "COM"
    let tree' = (const 1) <$> tree
    let lv = levels tree
    print $ sum $ (\x -> fst x * length (snd x)) <$> (zip [0..] lv)
