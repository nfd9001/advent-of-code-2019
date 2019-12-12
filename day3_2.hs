import qualified Data.Set as Set
import Data.List 
import Data.Maybe
import Control.Monad.Writer.Lazy
import Text.Show.Functions

data Direction = DUp Int | DDown Int | DLeft Int | DRight Int
    deriving (Show)
dir ('U':xs) = DUp    $ read xs
dir ('D':xs) = DDown  $ read xs
dir ('L':xs) = DLeft  $ read xs
dir ('R':xs) = DRight $ read xs
dir _      = error "Failed to parse direction"

points (DUp n)    (x, y) = [(x, y+i) | i <- [1..n]]
points (DDown n)  (x, y) = [(x, y-i) | i <- [1..n]]
points (DLeft n)  (x, y) = [(x-i, y) | i <- [1..n]]
points (DRight n) (x, y) = [(x+i, y) | i <- [1..n]]

manhattan (p, p') (q, q') = (abs $ p - q) + (abs $ p' - q')

step :: (Int, Int) -> Direction -> Writer [(Int, Int)] (Int, Int) 
step (x, y) dir = let p = points dir (x, y) in do
    tell p
    return $ last p

turnsToList :: [Direction] -> [(Int, Int)]
turnsToList = execWriter . foldM step (0, 0)
main = do
    f     <- readFile "day3.txt"
    let ls = lines f
    let turnses = do
        l <- ls
        return $ dir <$> (words $ (\x -> if x == ',' then ' ' else  x )<$>l)
    let lists = turnsToList <$> turnses
    let sets = Set.fromList <$> lists
    let allIntersections = Set.toList $ foldl1' Set.intersection sets

    let rs = do
        i <- allIntersections
        --fromJust is safe because all lists have all points in allIntersections
        return $ sum $ fromJust . elemIndex i <$> lists
        
    print $ 2 + minimum rs
