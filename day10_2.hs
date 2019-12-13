{-# LANGUAGE TupleSections #-}
import Control.Monad 
import Data.List
import Data.Tuple
import qualified Data.Set as Set

angle (x1, y1) (x2, y2) = let 
    --yes, really x then y
    a = atan2 (fromIntegral $ x2 - x1) (-(fromIntegral $ y2 - y1))
    in if a < 0 then 2 * pi + a else a

distance (x1, y1) (x2, y2) = sqrt((fromIntegral $ x1 - x2)**2 + (fromIntegral $ y1 - y2)**2)
interleave = concat . transpose
    
main = do
    f <- readFile "day10exlg.txt"
    let nls = zip [0..] $ fmap (\x -> fst x) . filter (\x -> snd x == '#') . zip [0..] <$> lines f
    let g (x,ys) = do y <- ys; return (x, y)
    let pairs = swap <$> (join $ g <$> nls :: [(Integer, Integer)])
    --let best = (21, 20) 
    let best = (11,13) 
    let rest = delete best pairs
    
    let i = groupBy (\x y -> snd x == snd y) $ sortOn snd $(\x -> (x, angle best x)) <$> rest
    let j = do
        l <- i
        return $ sortOn (distance best . fst) l
    let a = interleave j 
    print a
    print $ a!!200
