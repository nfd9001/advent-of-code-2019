import Data.Set (Set, toList, fromList, intersection)
import Data.List (sortOn)
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

step :: (Int, Int) -> Direction -> Writer (Set (Int, Int)) (Int, Int) 
step (x, y) dir = let p = points dir (x, y) in do
    tell $ fromList p
    return $ last p

turnsToSet :: [Direction] -> Set (Int, Int)
turnsToSet = execWriter . foldM step (0, 0)
main = do
    f     <- readFile "day3.txt"
    --turnses <- undefined -- return $ fmap (dir $ words (fmap (\x -> if x == ',' then ' ' else x))) (lines f)
    ls <- return $ lines f
    turnses <- return $ do
        l <- ls
        return $ dir <$> (words $ (\x -> if x == ',' then ' ' else  x )<$>l)
    sets <- return $ turnsToSet <$> turnses
    i <- return $ foldl1 intersection sets
    j <- return $ sortOn (manhattan (0,0)) $ toList i
    putStrLn $ show $ manhattan (0,0) (head j)
